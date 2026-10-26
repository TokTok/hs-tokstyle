{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Strict         #-}
module Tokstyle.C.Env
    ( DiagnosticLevel(..)
    , DiagnosticSpan(..)
    , CPosition(..)
    , LinterError
    , Env(..)
    , defaultEnv
    , bracketUserState
    , getCtx
    , pushCtx
    , popCtx
    , getRetTy
    , setRetTy
    , unsetRetTy
    , recordLinterError
    , recordRichError
    , safePosFile
    , posAndLen
    ) where

import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Text                     (Text)
import           Language.C.Analysis.SemRep    (GlobalDecls, IdentDecl, Type)
import           Language.C.Analysis.TravMonad (Trav, getUserState,
                                                modifyUserState)
import           Language.C.Data.Ident         (Ident, SUERef)
import           Language.C.Data.Node          (NodeInfo (..), nodeInfo)
import           Language.C.Data.Position      as C
import           Language.Cimple.Diagnostics   (Diagnostic (..),
                                                DiagnosticLevel (..),
                                                DiagnosticSpan (..),
                                                IsPosition (..))
import           Prettyprinter                 (Doc)

import           Prettyprinter.Render.Terminal (AnsiStyle)
import           Tokstyle.C.Analysis.Interval  (Program, emptyProgram)
import           Tokstyle.C.Analysis.Program   (Analysed)
import           Tokstyle.C.TravUtils          (getJust)

newtype CPosition = CPosition { unCPosition :: Position }
    deriving (Show, Eq)

instance IsPosition CPosition where
    posFile = C.posFile . unCPosition
    posLine = C.posRow . unCPosition
    posColumn = C.posColumn . unCPosition
    isRealPos = C.isSourcePos . unCPosition

type LinterError = Diagnostic CPosition

data Env = Env
    { ctx           :: [String]
    , retTy         :: Maybe Type
    , params        :: [Ident]
    , inferredTypes :: Map Ident Type
    , mainTypedefs  :: Map SUERef Ident
    , globalDecls   :: Maybe GlobalDecls
    , linterErrors  :: [LinterError]
    , currentFlag   :: Maybe Text
    , program       :: Program
    -- ^ What is known about the program the translation unit is part of.
    , localsOf      :: Map String [IdentDecl]
    -- ^ The locals each function declares, as analysing the translation unit
    -- found them.
    , pendingLocals :: [IdentDecl]
    -- ^ The locals of the function being analysed, in reverse.
    , analysed      :: [Analysed]
    -- ^ The functions of the translation unit that can wrap around, analysed
    -- knowing nothing about the rest of the program.
    }

defaultEnv :: Env
defaultEnv = Env ["file"] Nothing [] Map.empty Map.empty Nothing [] Nothing emptyProgram Map.empty [] []


bracketUserState :: (Env -> Env) -> Trav Env a -> Trav Env a
bracketUserState f act = do
    s <- getUserState
    modifyUserState f
    r <- act
    modifyUserState $ \s' -> s{linterErrors = linterErrors s'}
    return r


getCtx :: Trav Env [String]
getCtx = ctx <$> getUserState

pushCtx :: String -> Trav Env ()
pushCtx s = modifyUserState $ \env@Env{ctx} -> env{ctx = s:ctx}

popCtx :: Trav Env ()
popCtx = modifyUserState $ \env@Env{ctx} -> env{ctx = drop 1 ctx}


getRetTy :: Trav Env Type
getRetTy = getUserState >>= getJust "not in function context" . retTy

setRetTy :: Type -> Trav Env ()
setRetTy t = modifyUserState $ \env -> env{retTy = Just t}

unsetRetTy :: Trav Env ()
unsetRetTy = modifyUserState $ \env -> env{retTy = Nothing}

recordLinterError :: NodeInfo -> Doc AnsiStyle -> Trav Env ()
recordLinterError info doc = modifyUserState $ \env ->
    let (pos, len) = posAndLen info
    in env{linterErrors = Diagnostic (CPosition pos) len ErrorLevel doc (currentFlag env) [] [] : linterErrors env}

recordRichError :: NodeInfo -> DiagnosticLevel -> Doc AnsiStyle -> [DiagnosticSpan Position] -> [(DiagnosticLevel, Doc AnsiStyle)] -> Trav Env ()
recordRichError info level msg spans footer = modifyUserState $ \env ->
    let (pos, len) = posAndLen info
        wrapSpan s = s { spanPos = CPosition (spanPos s) }
    in env{linterErrors = Diagnostic (CPosition pos) len level msg (currentFlag env) (map wrapSpan spans) footer : linterErrors env}

safePosFile :: Position -> String
safePosFile p | C.isSourcePos p = C.posFile p
              | isNoPos p = "<no file>"
              | isBuiltinPos p = "<builtin>"
              | isInternalPos p = "<internal>"
              | otherwise = "<unknown>"

posAndLen :: NodeInfo -> (Position, Int)
posAndLen info = case nodeInfo info of
    NodeInfo pos (lastPos, lastLen) _ -> (pos, calcLen pos lastPos lastLen)
    OnlyPos pos (lastPos, lastLen)    -> (pos, calcLen pos lastPos lastLen)
  where
    calcLen pos lastPos lastLen =
        if C.isSourcePos pos && C.isSourcePos lastPos && safePosFile pos == safePosFile lastPos && C.posRow pos == C.posRow lastPos
        then (C.posColumn lastPos + lastLen) - C.posColumn pos
        else lastLen
