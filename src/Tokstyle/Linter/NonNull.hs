{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE ViewPatterns      #-}
module Tokstyle.Linter.NonNull (descr) where

import           Control.Arrow               ((&&&))
import           Control.Monad               (forM_, when)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Maybe                  (isJust)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (AlexPosn (..), Lexeme (..),
                                              LexemeClass (..), Node,
                                              NodeF (..), Scope (..),
                                              lexemeText)
import           Language.Cimple.Diagnostics (HasDiagnostics (..), warn)
import           Language.Cimple.Pretty      (showNode)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Tokstyle.Common             (isPointer)


data Linter = Linter
    { diags   :: [Text]
    , statics :: [(Text, (Lexeme Text, Bool))]
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
    case superfluous (nonnull ++ nullable) of
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


isDestructor :: Lexeme Text -> Bool
isDestructor name =
    "_free" `Text.isSuffixOf` lexemeText name ||
    "_kill" `Text.isSuffixOf` lexemeText name ||
    "kill_" `Text.isPrefixOf` lexemeText name

hasPerParamAnnotation :: Node (Lexeme Text) -> Bool
hasPerParamAnnotation (Fix (NonNullParam _))  = True
hasPerParamAnnotation (Fix (NullableParam _)) = True
hasPerParamAnnotation _                       = False

isPerParamNonNull :: Node (Lexeme Text) -> Bool
isPerParamNonNull (Fix (NonNullParam _)) = True
isPerParamNonNull _                      = False

getVarDeclName :: Node (Lexeme Text) -> Lexeme Text
getVarDeclName (Fix (VarDecl _ name _)) = name
getVarDeclName (Fix (NonNullParam p))   = getVarDeclName p
getVarDeclName (Fix (NullableParam p))  = getVarDeclName p
getVarDeclName (Fix n)                  = L (AlexPn 0 0 0) IdVar . Text.pack . show . fmap (const ()) $ n

checkPerParamAnns :: FilePath -> Lexeme Text -> Scope -> [Node (Lexeme Text)] -> Maybe (Node (Lexeme Text)) -> State Linter ()
checkPerParamAnns file name scope params mBody = do
    forM_ params $ \p ->
        when (hasPerParamAnnotation p && not (isPointer p)) $
            let vardeclName = getVarDeclName p
            in warn file vardeclName $ "nullability attribute on non-pointer parameter `" <> lexemeText vardeclName <> "`"

    when (isDestructor name && length params == 1 && any isPerParamNonNull params) $
        warn file name $ "destructor function `" <> lexemeText name <> "` must accept nullable arguments"

    when (isJust mBody && scope == Global && any hasPerParamAnnotation params) $
        warn file name "global function must only have nullability attribute on its declaration, not on its definition"

    when (isJust mBody && scope == Static) $ do
        Linter{statics} <- State.get
        case lookup (lexemeText name) statics of
            Nothing -> return ()
            Just (prev, hasAnn) ->
                when (any hasPerParamAnnotation params && not hasAnn) $ do
                   warn file name "static function must have nullability attribute on its declaration if it has one"
                   warn file prev "  declaration was here"

linter :: AstActions (State Linter) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            NonNull nonnull nullable (Fix fn) -> case fn of
                FunctionDefn scope (Fix (FunctionPrototype _ name params)) body -> do
                    if any hasPerParamAnnotation params then
                        warn file name "cannot mix function-level and per-parameter nullability attributes"
                    else do
                        when (not $ any isPointer params) $
                            warn file name "function has no pointer-type parameters, nullability has no effect"
                        when (scope == Global) $
                            warn file name "global function must only have nullability attribute on its declaration, not on its definition"
                        when (scope == Static) $ do
                            Linter{statics} <- State.get
                            case lookup (lexemeText name) statics of
                                Nothing -> return ()
                                Just (prev, hasAnn) ->
                                    when (not hasAnn) $ do
                                        warn file name "static function must have nullability attribute on its declaration if it has one"
                                        warn file prev "  declaration was here"
                        checkParams file nonnull nullable params
                    traverseAst linter (file, [body])
                FunctionDecl scope (Fix (FunctionPrototype _ name params)) -> do
                    when (scope == Static) $
                        State.modify $ \l@Linter{statics} -> l{statics = (lexemeText name, (name, True)) : statics}
                    if any hasPerParamAnnotation params then
                        warn file name "cannot mix function-level and per-parameter nullability attributes"
                    else do
                        when (nonnull == [] && nullable == [] && isDestructor name && length params == 1 && any isPointer params) $
                            warn file name $ "destructor function `" <> lexemeText name <> "` must accept nullable arguments"
                        checkParams file nonnull nullable params
                        when (not $ any isPointer params) $
                            warn file name "function declaration has no pointer-type parameters, nullability has no effect"
                _ -> act

            FunctionDefn scope (Fix (FunctionPrototype _ name params)) body -> do
                when (any hasPerParamAnnotation params) $
                    checkPerParamAnns file name scope params (Just body)
                act

            FunctionDecl Static (Fix (FunctionPrototype _ name params)) -> do
                State.modify $ \l@Linter{statics} -> l{statics = (lexemeText name, (name, any hasPerParamAnnotation params)) : statics}
                when (any hasPerParamAnnotation params) $
                    checkPerParamAnns file name Static params Nothing
                act

            FunctionDecl scope (Fix (FunctionPrototype _ name params)) -> do
                when (any hasPerParamAnnotation params) $
                    checkPerParamAnns file name scope params Nothing
                act

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . diags . flip State.execState empty . traverseAst linter

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("non-null", Text.unlines
    [ "Checks that all pointer parameters are listed in either `non_null` or"
    , "`nullable`, and that none of the numbers in these annotations are non-pointers."
    , ""
    , "**Reason:** see `-Wmissing-non-null` for more context. This check ensures that"
    , "nullability annotations are updated when parameter lists change."
    ]))
