{-# LANGUAGE LambdaCase #-}
-- | The parts of statements and expressions.
module Tokstyle.C.Analysis.Syntax
    ( exprChildren
    , statExprs
    , initExprs
    , localDecls
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

-- | Every expression a statement evaluates, including in nested statements.
statExprs :: CStat -> [CExpr]
statExprs = \case
    CExpr e _ -> maybe [] pure e
    CReturn e _ -> maybe [] pure e
    CCompound _ items _ -> concatMap item items
    CIf c t e _ -> c : statExprs t ++ maybe [] statExprs e
    CSwitch c s _ -> c : statExprs s
    CWhile c s _ _ -> c : statExprs s
    CFor i c st s _ -> either (maybe [] pure) declExprs i ++ maybe [] pure c ++ maybe [] pure st ++ statExprs s
    CLabel _ s _ _ -> statExprs s
    CCase e s _ -> e : statExprs s
    CCases a b s _ -> a : b : statExprs s
    CDefault s _ -> statExprs s
    _ -> []
  where
    item = \case
        CBlockDecl d -> declExprs d
        CBlockStmt s -> statExprs s
        _            -> []

declExprs :: CDecl -> [CExpr]
declExprs = \case
    CDecl _ ds _ -> concat [ initExprs ini | (_, Just ini, _) <- ds ]
    _            -> []

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

-- | The declarations in a function body, in the order they appear.
localDecls :: CStat -> [CDecl]
localDecls = \case
    CCompound _ items _ -> concatMap item items
    CIf _ t e _         -> localDecls t ++ maybe [] localDecls e
    CSwitch _ s _       -> localDecls s
    CWhile _ s _ _      -> localDecls s
    CFor i _ _ s _      -> either (const []) pure i ++ localDecls s
    CLabel _ s _ _      -> localDecls s
    CCase _ s _         -> localDecls s
    CCases _ _ s _      -> localDecls s
    CDefault s _        -> localDecls s
    _                   -> []
  where
    item = \case
        CBlockDecl d -> [d]
        CBlockStmt s -> localDecls s
        _            -> []
