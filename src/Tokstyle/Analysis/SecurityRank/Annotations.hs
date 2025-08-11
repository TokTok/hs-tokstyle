{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tokstyle.Analysis.SecurityRank.Annotations
    ( AnnotationMap
    , FunctionAnnotation
    , parseAllAnnotations
    ) where

import           Control.Monad                          (when)
import           Control.Monad.State.Strict             (State, execState, get,
                                                         gets, modify, modify')
import           Data.Fix                               (Fix (..))
import           Data.Map.Strict                        (Map)
import qualified Data.Map.Strict                        as Map
import           Data.Maybe                             (isJust, mapMaybe)
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           Language.Cimple                        (CommentF (..),
                                                         NodeF (..))
import qualified Language.Cimple                        as C
import           Language.Cimple.TraverseAst            (AstActions (..),
                                                         astActions,
                                                         traverseAst)
import           Tokstyle.Analysis.SecurityRank.Lattice (SecurityRank (..))

-- | Maps a parameter name (or "return", "sink", "sink_max") to its rank.
type FunctionAnnotation = Map Text SecurityRank

-- | Maps a function or struct member name to its full annotation.
type AnnotationMap = Map Text FunctionAnnotation

data CollectorState = CollectorState
    { csAnnotations   :: AnnotationMap
    , csCurrentStruct :: Maybe Text -- Still needed to construct the full member name.
    }

-- | Extracts security rank annotations from a list of comment items.
extractRanksFromItems :: [C.Comment (C.Lexeme Text)] -> [(Text, SecurityRank)]
extractRanksFromItems = mapMaybe $ \(Fix item) -> case item of
    DocSecurityRank (C.L _ _ kw) mparam (C.L _ _ rankStr) ->
        let
            rankVal = read (T.unpack rankStr)
            key = case mparam of
                    Nothing              -> kw
                    Just (C.L _ _ param) -> kw <> ":" <> param
        in
            Just (key, Rank rankVal)
    _ -> Nothing

-- | Safely extracts the list of items from a DocLine, returning empty for other types.
getDocLineItems :: C.Comment (C.Lexeme Text) -> [C.Comment (C.Lexeme Text)]
getDocLineItems (Fix (DocLine items)) = items
getDocLineItems _                     = []

-- | Traverses the AST to find commented declarations and parse their annotations.
annotationActions :: AstActions (State CollectorState) Text
annotationActions = astActions
    { doNode = \_file node act -> do
        originalStruct <- gets csCurrentStruct

        let mNewStruct = case unFix node of
                C.Struct (C.L _ _ name) _                     -> Just name
                C.Typedef (Fix (C.Struct (C.L _ _ name) _)) _ -> Just name
                _                                             -> Nothing

        -- If this node defines a new struct context, update the state before processing it and its children.
        when (isJust mNewStruct) $ do
            modify $ \st -> st { csCurrentStruct = mNewStruct }

        -- Process the current node using the potentially updated context.
        processNode node

        -- Traverse into children.
        act

        -- After traversing, restore the original context.
        modify $ \st -> st { csCurrentStruct = originalStruct }
    }

-- | Processes a single node to find and store annotations.
processNode :: C.Node (C.Lexeme Text) -> State CollectorState ()
processNode (Fix (C.Commented (Fix (C.CommentInfo (Fix (C.DocComment docLines)))) decl)) = do
    st <- get
    let allItems = concatMap getDocLineItems docLines
    let ranks = extractRanksFromItems allItems
    let annotation = Map.fromList ranks

    -- Handle function annotations
    case unFix decl of
        C.FunctionDefn _ (Fix (C.FunctionPrototype _ (C.L _ _ name) _)) _ ->
            addAnnotations name annotation
        C.FunctionDecl _ (Fix (C.FunctionPrototype _ (C.L _ _ name) _)) ->
            addAnnotations name annotation
        C.TypedefFunction (Fix (C.FunctionPrototype _ (C.L _ _ name) _)) ->
            addAnnotations name annotation
        _ -> return ()

    -- Handle struct member annotations
    case (unFix decl, csCurrentStruct st) of
        (C.MemberDecl (Fix (C.VarDecl _ (C.L _ _ memberName) _)) _, Just structName) -> do
            let fullMemberName = structName <> "." <> memberName
            addAnnotations fullMemberName annotation
        _ -> return ()
processNode _ = return ()


-- | Adds a list of parsed annotations for a given entity (function or struct member).
addAnnotations :: Text -> FunctionAnnotation -> State CollectorState ()
addAnnotations name annotation =
    modify' $ \st -> st { csAnnotations = Map.insert name annotation (csAnnotations st) }

-- | The main entry point. Traverses all translation units and returns the complete
-- annotation map.
parseAllAnnotations :: [(FilePath, [C.Node (C.Lexeme Text)])] -> AnnotationMap
parseAllAnnotations tus =
    let
        initialState = CollectorState Map.empty Nothing
        finalState = execState (traverseAst annotationActions tus) initialState
    in
        csAnnotations finalState
