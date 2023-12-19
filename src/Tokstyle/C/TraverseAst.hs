{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Tokstyle.C.TraverseAst where

import           Data.Foldable              (for_, traverse_)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Language.C
import           Language.C.Analysis.SemRep

class TraverseAst a where
    traverseAst
        :: Applicative f
        => AstActions f
        -> a
        -> f ()

data AstActions f = AstActions
    { doGlobalDecls ::  GlobalDecls  -> f () -> f ()
    , doIdentDecl   ::  IdentDecl    -> f () -> f ()
    , doConst       ::  CConst       -> f () -> f ()
    , doInit        ::  CInit        -> f () -> f ()
    , doStat        ::  CStat        -> f () -> f ()
    , doStats       :: [CStat]       -> f () -> f ()
    , doExpr        ::  CExpr        -> f () -> f ()
    , doExprs       :: [CExpr]       -> f () -> f ()
    , doDecl        ::  CDecl        -> f () -> f ()
    , doDecls       :: [CDecl]       -> f () -> f ()
    , doBlockItem   ::  CBlockItem   -> f () -> f ()
    , doBlockItems  :: [CBlockItem]  -> f () -> f ()
    }

astActions :: Applicative f => AstActions f
astActions = AstActions
    { doGlobalDecls = const id
    , doIdentDecl   = const id
    , doConst       = const id
    , doInit        = const id
    , doStat        = const id
    , doStats       = const id
    , doExpr        = const id
    , doExprs       = const id
    , doDecl        = const id
    , doDecls       = const id
    , doBlockItem   = const id
    , doBlockItems  = const id
    }

instance TraverseAst a => TraverseAst (Maybe a) where
    traverseAst _ Nothing        = pure ()
    traverseAst actions (Just x) = traverseAst actions x

instance (TraverseAst a, TraverseAst b) => TraverseAst (Either a b) where
    traverseAst actions (Left  a) = traverseAst actions a
    traverseAst actions (Right b) = traverseAst actions b

instance TraverseAst (Map Ident IdentDecl) where
    traverseAst actions decls = traverse_ (traverseAst actions) $ Map.elems decls


instance TraverseAst (FilePath, GlobalDecls) where
    traverseAst actions (_, decls) = traverseAst actions decls

instance TraverseAst GlobalDecls where
    traverseAst :: forall f. Applicative f => AstActions f -> GlobalDecls -> f ()
    traverseAst actions@AstActions{..} = doGlobalDecls <*> \case
        GlobalDecls{..} -> do
            _ <- recurse gObjs
            -- _ <- recurse gTags
            -- _ <- recurse gTypeDefs
            pure ()

      where
        recurse :: TraverseAst a => a -> f ()
        recurse = traverseAst actions


instance TraverseAst IdentDecl where
    traverseAst :: forall f. Applicative f => AstActions f -> IdentDecl -> f ()
    traverseAst actions@AstActions{..} = doIdentDecl <*> \case
        Declaration{} -> pure ()
        ObjectDef{} -> pure ()
        EnumeratorDef{} -> pure ()

        FunctionDef (FunDef _ s _) -> do
            _ <- recurse s
            pure ()

      where
        recurse :: TraverseAst a => a -> f ()
        recurse = traverseAst actions


instance TraverseAst CConst where
    traverseAst :: forall f. Applicative f => AstActions f -> CConst -> f ()
    traverseAst AstActions{..} = doConst <*> const (pure ())


instance TraverseAst CInit where
    traverseAst :: forall f. Applicative f => AstActions f -> CInit -> f ()
    traverseAst actions@AstActions{..} = doInit <*> \case
        CInitExpr e _ -> do
            _ <- recurse e
            pure ()
        CInitList{} -> pure ()

      where
        recurse :: TraverseAst a => a -> f ()
        recurse = traverseAst actions


instance TraverseAst CStat where
    traverseAst :: forall f. Applicative f => AstActions f -> CStat -> f ()
    traverseAst actions@AstActions{..} = doStat <*> \case
        CLabel _ s _ _ -> do
            _ <- recurse s
            pure ()

        CCase e s _ -> do
            _ <- recurse e
            _ <- recurse s
            pure ()

        CCases e1 e2 s _ -> do
            _ <- recurse e1
            _ <- recurse e2
            _ <- recurse s
            pure ()

        CDefault s _ -> do
            _ <- recurse s
            pure ()

        CExpr e _ -> do
            _ <- recurse e
            pure ()

        CCompound _ cbis _ -> do
            _ <- recurse cbis
            pure ()

        CIf cond t e _ -> do
            _ <- recurse cond
            _ <- recurse t
            _ <- recurse e
            pure ()

        CSwitch e s _ -> do
            _ <- recurse e
            _ <- recurse s
            pure ()

        CWhile e s _ _ -> do
            _ <- recurse e
            _ <- recurse s
            pure ()

        CFor i e2 e3 s _ -> do
            _ <- recurse i
            _ <- recurse e2
            _ <- recurse e3
            _ <- recurse s
            pure ()

        CGoto{} -> pure ()

        CGotoPtr e _ -> do
            _ <- recurse e
            pure ()

        CCont{} -> pure ()
        CBreak{} -> pure ()

        CReturn e _ -> do
            _ <- recurse e
            pure ()

        CAsm{} -> do
            pure ()

      where
        recurse :: TraverseAst a => a -> f ()
        recurse = traverseAst actions

instance TraverseAst [CStat] where
    traverseAst actions@AstActions{..} = doStats <*> traverse_ (traverseAst actions)


instance TraverseAst CExpr where
    traverseAst :: forall f. Applicative f => AstActions f -> CExpr -> f ()
    traverseAst actions@AstActions{..} = doExpr <*> \case
        CComma es _ -> do
            _ <- recurse es
            pure ()

        CAssign _ l r _ -> do
            _ <- recurse l
            _ <- recurse r
            pure ()

        CCond c t e _ -> do
            _ <- recurse c
            _ <- recurse t
            _ <- recurse e
            pure ()

        CBinary _ l r _ -> do
            _ <- recurse l
            _ <- recurse r
            pure ()

        CCast t e _ -> do
            _ <- recurse t
            _ <- recurse e
            pure ()

        CUnary _ e _ -> do
            _ <- recurse e
            pure ()

        CSizeofExpr e _ -> do
            _ <- recurse e
            pure ()

        CSizeofType t _ -> do
            _ <- recurse t
            pure ()

        CAlignofExpr e _ -> do
            _ <- recurse e
            pure ()

        CAlignofType t _ -> do
            _ <- recurse t
            pure ()

        CComplexReal e _ -> do
            _ <- recurse e
            pure ()

        CComplexImag e _ -> do
            _ <- recurse e
            pure ()

        CIndex e i _ -> do
            _ <- recurse e
            _ <- recurse i
            pure ()

        CCall f args _ -> do
            _ <- recurse f
            _ <- recurse args
            pure ()

        CMember e _ _ _ -> do
            _ <- recurse e
            pure ()

        CVar{} -> do
            pure ()

        CConst c -> do
            _ <- recurse c
            pure ()

        CCompoundLit{} -> do
            pure ()

        CGenericSelection{} -> do
            pure ()

        CStatExpr s _ -> do
            _ <- recurse s
            pure ()

        CLabAddrExpr{} -> do
            pure ()

        CBuiltinExpr{} -> do
            pure ()

      where
        recurse :: TraverseAst a => a -> f ()
        recurse = traverseAst actions

instance TraverseAst [CExpr] where
    traverseAst actions@AstActions{..} = doExprs <*> traverse_ (traverseAst actions)


instance TraverseAst CDecl where
    traverseAst :: forall f. Applicative f => AstActions f -> CDecl -> f ()
    traverseAst actions@AstActions{..} = doDecl <*> \case
        CDecl _ ds _ ->
            for_ ds $ \(_, i, e) -> do
                _ <- recurse i
                _ <- recurse e
                pure ()

        CStaticAssert e _ _ -> do
            _ <- recurse e
            pure ()

      where
        recurse :: TraverseAst a => a -> f ()
        recurse = traverseAst actions

instance TraverseAst [CDecl] where
    traverseAst actions@AstActions{..} = doDecls <*> traverse_ (traverseAst actions)


instance TraverseAst CBlockItem where
    traverseAst :: forall f. Applicative f => AstActions f -> CBlockItem -> f ()
    traverseAst actions@AstActions{..} = doBlockItem <*> \case
        CBlockStmt s -> do
            _ <- recurse s
            pure ()

        CBlockDecl d -> do
            _ <- recurse d
            pure ()

        x -> error $ show x

      where
        recurse :: TraverseAst a => a -> f ()
        recurse = traverseAst actions

instance TraverseAst [CBlockItem] where
    traverseAst actions@AstActions{..} = doBlockItems <*> traverse_ (traverseAst actions)
