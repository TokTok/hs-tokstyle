{-# LANGUAGE Strict #-}
module Tokstyle.Common
    ( functionName
    , isPointer
    , semEq
    ) where

import           Data.Fix        (Fix (..))
import           Data.Text       (Text)
import           Language.Cimple (Lexeme (..), LexemeClass (..), Node,
                                  NodeF (..), removeSloc)


isPointer :: Node (Lexeme Text) -> Bool
isPointer x = case unFix x of
    VarDecl ty _ [] -> isPointer ty
    VarDecl{}       -> True
    TyConst ty      -> isPointer ty
    TyPointer{}     -> True
    TyStd{}         -> False
    TyStruct{}      -> False
    TyUserDefined{} -> False
    _               -> error $ show x


-- | Extract the name of a function, possibly inside an attribute node.
--
-- Non-function nodes result in 'Nothing'.
functionName :: Show a => Node (Lexeme a) -> Maybe a
functionName (Fix (FunctionPrototype _ (L _ IdVar name) _)) = Just name
functionName (Fix (FunctionDecl _ proto  )) = functionName proto
functionName (Fix (FunctionDefn _ proto _)) = functionName proto
functionName (Fix (AttrPrintf _ _ entity))  = functionName entity
functionName (Fix (NonNull _ _ entity))     = functionName entity
functionName _                              = Nothing


-- Semantic equality: nodes are the same, except for source locations.
semEq :: Node (Lexeme Text) -> Node (Lexeme Text) -> Bool
semEq a b = removeSloc a == removeSloc b
