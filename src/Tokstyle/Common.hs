{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict           #-}
module Tokstyle.Common
    ( functionName
    , isPointer
    , isPublicHeader
    , semEq
    , skip
    , warn
    , warnDoc
    , backticks
    , dquotes
    , (>+>)
    ) where

import           Data.Fix                      (Fix (..))
import qualified Data.List                     as List
import           Data.Text                     (Text)
import           Language.Cimple               (Lexeme (..), LexemeClass (..),
                                                Node, NodeF (..), removeSloc)
import           Language.Cimple.Diagnostics   (CimplePos, Diagnostic (..),
                                                DiagnosticLevel (..),
                                                DiagnosticsT,
                                                HasDiagnosticInfo (..),
                                                HasDiagnosticsRich, warnRich)
import           Prettyprinter                 (Doc, pretty)
import           Prettyprinter.Render.Terminal (AnsiStyle)


isPointer :: Node (Lexeme Text) -> Bool
isPointer x = case unFix x of
    VarDecl ty _ []  -> isPointer ty
    VarDecl{}        -> True
    TyConst ty       -> isPointer ty
    TyOwner ty       -> isPointer ty
    TyPointer{}      -> True
    TyStd{}          -> False
    TyStruct{}       -> False
    TyUserDefined{}  -> False
    NonNullParam ty  -> isPointer ty
    NullableParam ty -> isPointer ty
    _                -> error $ show x


-- | Extract the name of a function, possibly inside an attribute node.
--
-- Non-function nodes result in 'Nothing'.
functionName :: Show a => Node (Lexeme a) -> Maybe a
functionName (Fix (FunctionPrototype _ (L _ _ name) _)) = Just name
functionName (Fix (FunctionDecl _ proto  ))             = functionName proto
functionName (Fix (FunctionDefn _ proto _))             = functionName proto
functionName (Fix (AttrPrintf _ _ entity))              = functionName entity
functionName (Fix (NonNull _ _ entity))                 = functionName entity
functionName _                                          = Nothing


-- Semantic equality: nodes are the same, except for source locations.
semEq :: Node (Lexeme Text) -> Node (Lexeme Text) -> Bool
semEq a b = removeSloc a == removeSloc b


-- Don't apply the linter to certain files.
skip :: [FilePath] -> (FilePath, [Node (Lexeme Text)]) -> (FilePath, [Node (Lexeme Text)])
skip fps (fp, _) | any (`List.isSuffixOf` fp) fps = (fp, [])
skip _ tu        = tu

warn :: (HasDiagnosticsRich diags CimplePos, HasDiagnosticInfo at CimplePos) => FilePath -> at -> Text -> DiagnosticsT diags ()
warn file at msg = warnDoc file at (pretty msg)

warnDoc :: (HasDiagnosticsRich diags CimplePos, HasDiagnosticInfo at CimplePos) => FilePath -> at -> Doc AnsiStyle -> DiagnosticsT diags ()
warnDoc file at doc =
    let (pos, len) = getDiagnosticInfo file at
    in warnRich $ Diagnostic pos len WarningLevel doc Nothing [] []

backticks :: Doc ann -> Doc ann
backticks d = pretty '`' <> d <> pretty '`'

dquotes :: Doc ann -> Doc ann
dquotes d = pretty '"' <> d <> pretty '"'

(>+>) :: Monad m => (t -> m ()) -> (t -> m ()) -> t -> m ()
(>+>) f g x = f x >> g x


isPublicHeader :: FilePath -> Bool
isPublicHeader path = any (`List.isSuffixOf` path) ["tox.h", "tox_events.h", "tox_dispatch.h", "toxav.h", "toxencryptsave.h", "tox_options.h", "tox_log_level.h"]
