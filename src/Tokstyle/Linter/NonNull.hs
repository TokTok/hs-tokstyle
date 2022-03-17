{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE ViewPatterns      #-}
module Tokstyle.Linter.NonNull (analyse) where

import           Control.Arrow               ((&&&))
--import           Control.Monad               (when)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..),
                                              Scope (..), lexemeText)
import           Language.Cimple.Diagnostics (HasDiagnostics (..), warn)
import           Language.Cimple.Pretty      (showNode)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Tokstyle.Common             (isPointer)


data Linter = Linter
    { diags   :: [Text]
    , statics :: [(Text, Lexeme Text)]
    }

empty :: Linter
empty = Linter [] []

instance HasDiagnostics Linter where
    addDiagnostic diag l@Linter{diags} = l{diags = addDiagnostic diag diags}

indices :: [Lexeme Text] -> [(Int, Lexeme Text)]
indices = map (read . Text.unpack . lexemeText &&& id)

paramMap :: [Node (Lexeme Text)] -> [(Int, Node (Lexeme Text))]
paramMap = filter (isPointer . snd) . zip [1..]

checkParams :: FilePath -> [Lexeme Text] -> [Lexeme Text] -> [Node (Lexeme Text)] -> State Linter ()
checkParams _ [] [] _ = return ()  -- non_null() marks all params as non-null
checkParams file (indices -> nonnull) (indices -> nullable) params = do
    case unmarked ptrParams of
        [] -> return ()
        (ix, l):_ -> warn file l $ "pointer-type parameter " <> Text.pack (show ix) <> " (`" <> showNode l <> "`) has no non_null or nullable attribute"
    case superfluous nullable of
        [] -> return ()
        (ix, l):_ -> warn file l $ "parameter " <> lexemeText l <> " (" <> showParam ix <> ") does not have a pointer type; nullability has no effect"
  where
    ptrParams = paramMap params
    unmarked = filter (not . (`elem` map fst nonnull ++ map fst nullable) . fst)
    superfluous = filter (not . (`elem` map fst ptrParams) . fst)

    showParam :: Int -> Text
    showParam ix =
        case drop (ix - 1) params of
          [] -> "only " <> Text.pack (show (length params)) <> " parameters available"
          p:_ -> "`" <> showNode p <> "`"


linter :: AstActions (State Linter) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            FunctionDecl Static (Fix (FunctionPrototype _ name _)) ->
                State.modify $ \l@Linter{statics} -> l{statics = (lexemeText name, name) : statics}

            NonNull nonnull nullable (Fix (FunctionDefn Static (Fix (FunctionPrototype _ name params)) _)) -> do
                checkParams file nonnull nullable params
                Linter{statics} <- State.get
                case lookup (lexemeText name) statics of
                    Nothing -> return ()
                    Just prev -> do
                       warn file name "static function must have nullability attribute on its declaration if it has one"
                       warn file prev "  declaration was here"

            NonNull _ _ (Fix (FunctionDefn _ (Fix (FunctionPrototype _ name params)) _)) | not $ any isPointer params ->
               warn file name "function definition has no pointer-type parameters, nullability has no effect"
            NonNull _ _ (Fix (FunctionDecl _ (Fix (FunctionPrototype _ name params)))) | not $ any isPointer params ->
               warn file name "function declaration has no pointer-type parameters, nullability has no effect"

            NonNull _ _ (Fix (FunctionDefn Global (Fix (FunctionPrototype _ name _)) _)) ->
               warn file name "global function must only have nullability attribute on its declaration, not on its definition"

            NonNull nonnull nullable (Fix (FunctionDecl _ (Fix (FunctionPrototype _ _ params)))) ->
                checkParams file nonnull nullable params

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . diags . flip State.execState empty . traverseAst linter
