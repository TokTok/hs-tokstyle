{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.Constness (descr) where

import           Control.Applicative         ((<|>))
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..), foldFix)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import qualified Data.Maybe                  as Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..),
                                              UnaryOp (..), lexemeText)
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)

-- | Specifies what is happening to a variable.
--
-- This enum has a total order of importance for combine and later filter.
--
-- Anything >=Write is good. Anything <=Read is bad.
data Action
    = Declare
      -- ^ The variable is never read or written. We don't warn about this
      -- because other linters already do.
    | Read
      -- ^ The variable is read. If it was locally declared and only read, then
      -- it was not declared const and should be.
    | Write
      -- ^ The variable is written to (and isn't const), so we don't warn about
      -- it.
    | Const
      -- ^ The variable is declared const, so everything is as we want it.
    deriving (Show, Eq, Ord)

-- | Variable declaration and usage.
--
-- If varDecl is empty, the variable wasn't locally declared so we ignore it.
data Var = Var
    { varUse  :: Action
    , varDecl :: Maybe (Lexeme Text)
    }
    deriving (Show, Eq)

combine :: Var -> Var -> Var
combine var1 var2 | var1 == var2 = var1
combine (Var use1 decl1) (Var use2 decl2) =
    Var (max use1 use2) (decl1 <|> decl2)

readToWrite :: Var -> Var
readToWrite var = var{varUse = Write}


constness :: NodeF (Lexeme Text) (Map Text Var) -> Map Text Var
constness = \case
    -- Only vardecls without array dimensions are added to the list. Arrays are
    -- considered not locally declared, so they are ignored.
    VarDecl t l@(L _ _ name) [] ->
        let var = Maybe.fromMaybe (Var Declare Nothing) $ Map.lookup "" t in
        Map.singleton name var{varDecl = Just l}

    VarExpr (L _ _ name)    -> readDecl name

    -- If it was declared const, we're good.
    -- We left-union with vars which may contain a fake-write created below for
    -- pointers and `va_list`.
    TyConst vars            -> Map.union vars constDecl
    -- Ignore `va_list` (by faking a write to pointer vars).
    TyStd (L _ _ "va_list") -> writeDecl
    -- Ignore pointers for now.
    TyPointer{}             -> writeDecl

    -- The array index isn't written to, so remove it from the list unless it
    -- was already written to elsewhere.
    ArrayAccess e i         -> Map.unionWith combine e (Map.filter ((>= Write) . varUse) i)

    -- Ignore function parameters for now.
    FunctionPrototype{}     -> Map.empty

    -- These expressions (potentially) write to vars.
    AssignExpr lhs _ rhs ->
        Map.unionWith combine rhs
        $ Map.map readToWrite lhs
    UnaryExpr uop expr | canWrite uop ->
        Map.map readToWrite expr

    n -> Map.unionsWith combine n
  where
    readDecl = flip Map.singleton (Var Read Nothing)
    constDecl = Map.singleton "" (Var Const Nothing)
    writeDecl = Map.singleton "" (Var Write Nothing)

    canWrite UopAddress = True
    canWrite UopDecr    = True
    canWrite UopIncr    = True
    canWrite _          = False


findCandidatesForConst :: Node (Lexeme Text) -> [Lexeme Text]
findCandidatesForConst =
    Maybe.mapMaybe varDecl
    . filter ((<= Read) . varUse)
    . Map.elems
    . foldFix constness


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            FunctionDefn{} ->
                let vars = findCandidatesForConst node in
                mapM_ (\var -> warn file var $
                    "variable `" <> lexemeText var
                    <> "` is never written to and can be declared `const`") vars

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("constness", Text.unlines
    [ "Warns if a variable can be marked as `const`, i.e. it is only initialised and"
    , "then never assigned again. Pointer types are exempt, i.e. `int *p = get_p();`"
    , "is fine and doesn't need to be written as `int *const p = get_p();`, but"
    , "`int q = get_q();`, if then `q` is never assigned again, should be written as"
    , "`const int q = get_q();`."
    , ""
    , "**Reason:** `const` makes the no-assign local invariant clear. We exempt pointer"
    , "types at the moment, because making that change in toxcore would be a lot of"
    , "work and we perceive less value in that than in local integer constants, since"
    , "pointers, especially aggregate object pointers, already change less often."
    ]))
