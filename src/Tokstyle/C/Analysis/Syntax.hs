{-# LANGUAGE LambdaCase #-}
-- | The parts of statements and expressions.
module Tokstyle.C.Analysis.Syntax
    ( exprChildren
    , initExprs
    , stats
    ) where

import           Language.C.Syntax.AST

exprChildren :: CExpr -> [CExpr]
exprChildren = \case
    CComma es _     -> es
    CAssign _ l r _ -> [l, r]
    CCond c t e _   -> c : maybe [] pure t ++ [e]
    CBinary _ l r _ -> [l, r]
    CCast _ e _     -> [e]
    CUnary _ e _    -> [e]
    CIndex b i _    -> [b, i]
    CCall f args _  -> f : args
    CMember e _ _ _ -> [e]
    CSizeofExpr e _ -> [e]
    _               -> []

initExprs :: CInit -> [CExpr]
initExprs = \case
    CInitExpr e _ -> [e]
    CInitList l _ -> concatMap (initExprs . snd) l

-- | The statements of a function body, recursively.
stats :: CStat -> [CStat]
stats s = s : case s of
    CLabel _ t _ _      -> stats t
    CCase _ t _         -> stats t
    CCases _ _ t _      -> stats t
    CDefault t _        -> stats t
    CCompound _ items _ -> concat [ stats t | CBlockStmt t <- items ]
    CIf _ t e _         -> stats t ++ maybe [] stats e
    CSwitch _ t _       -> stats t
    CWhile _ t _ _      -> stats t
    CFor _ _ _ t _      -> stats t
    _                   -> []
