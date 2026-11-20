{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.AccessorNames (descr) where

import           Control.Monad               (unless)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.List.NonEmpty          (NonEmpty (..))
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Maybe                  (mapMaybe)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (AssignOp (..), Lexeme (..), Node,
                                              NodeF (..))
import           Language.Cimple.Diagnostics (CimplePos, Diagnostic)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Prettyprinter               (pretty, (<+>))
import           Tokstyle.Common             (backticks, isPublicHeader,
                                              warnDoc)


data Access
    = Gets Text Text  -- ^ @return p->field;@
    | Sets Text Text  -- ^ @p->field = value;@ with @value@ another parameter.


publicFunctions :: [(FilePath, [Node (Lexeme Text)])] -> Set Text
publicFunctions = flip State.execState Set.empty . traverseAst collect
  where
    collect :: AstActions (State (Set Text)) Text
    collect = astActions
        { doNode = \file node act -> case unFix node of
            FunctionDecl _ (Fix (FunctionPrototype _ (L _ _ name) _))
                | isPublicHeader file -> State.modify (Set.insert name)
            _ -> act
        }


paramNames :: [Node (Lexeme Text)] -> [Text]
paramNames = mapMaybe name
  where
    name (Fix (VarDecl _ (L _ _ n) _)) = Just n
    name _                             = Nothing


access :: [Text] -> [Node (Lexeme Text)] -> Maybe Access
access params body = case map unFix body of
    [Return (Just (Fix (PointerAccess (Fix (VarExpr (L _ _ p))) (L _ _ field))))]
        | p `elem` params -> Just (Gets p field)
    [ExprStmt (Fix (AssignExpr (Fix (PointerAccess (Fix (VarExpr (L _ _ p))) (L _ _ field))) AopEq (Fix (VarExpr (L _ _ v)))))]
        | p `elem` params && v `elem` params && p /= v -> Just (Sets p field)
    _ -> Nothing


-- | Allowed name endings, the first of which we suggest.
expected :: Access -> NonEmpty Text
expected (Gets _ field) = ("_get_" <> field) :| []
expected (Sets _ field) = ("_set_" <> field) :| foldMap (pure . ("_callback_" <>)) (Text.stripSuffix "_callback" field)


linter :: Set Text -> AstActions (State [Diagnostic CimplePos]) Text
linter public = astActions
    { doNode = \file node act -> case unFix node of
        FunctionDefn _ (Fix (FunctionPrototype _ name@(L _ _ fname) params)) (Fix (CompoundStmt body))
            | fname `Set.member` public
            , Just acc <- access (paramNames params) body ->
                let ends = expected acc in
                unless (any (`Text.isSuffixOf` fname) ends) $
                    warnDoc file name $ describe acc fname
                        <> ", so its name should end in" <+> backticks (pretty (NonEmpty.head ends))
        _ -> act
    }
  where
    describe (Gets p field) fname =
        "getter" <+> backticks (pretty fname) <+> "returns" <+> backticks (pretty p <> "->" <> pretty field)
    describe (Sets p field) fname =
        "setter" <+> backticks (pretty fname) <+> "sets" <+> backticks (pretty p <> "->" <> pretty field)


analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse tus = reverse . flip State.execState [] . traverseAst (linter (publicFunctions tus)) $ tus

descr :: ([(FilePath, [Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("accessor-names", Text.unlines
    [ "Checks that getters and setters in the public API are named after the field"
    , "they access. E.g. `tox_options_get_proxy_host` should return"
    , "`options->proxy_host`, not `options->internal_proxy_host`. Only functions whose"
    , "body is just `return p->field;` or `p->field = value;` are checked. A setter of"
    , "a field `x_callback` may also be named `..._callback_x`."
    , ""
    , "**Reason:** this naming convention makes it clear at first glance which field"
    , "an accessor reads or writes."
    ]))
