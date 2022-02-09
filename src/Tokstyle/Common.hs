{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}
module Tokstyle.Common
    ( isPointer
    ) where

import           Data.Fix        (Fix (..))
import           Data.Text       (Text)
import           Language.Cimple (Lexeme (..), Node, NodeF (..))


isPointer :: Node (Lexeme Text) -> Bool
isPointer x = case unFix x of
    VarDecl ty _ [] -> isPointer ty
    VarDecl _ _ _   -> True
    TyConst ty      -> isPointer ty
    TyPointer{}     -> True
    TyStd{}         -> False
    TyUserDefined{} -> False
    _               -> error $ show x
