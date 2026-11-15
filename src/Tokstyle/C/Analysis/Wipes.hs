{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict     #-}
-- | Finds which locals get wiped, and which a call fills with data it makes
-- itself, e.g. a key from randomness.
module Tokstyle.C.Analysis.Wipes
    ( Arg
    , wipers
    , keepContents
    , wiped
    , subExprs
    , writableParams
    , declItems
    , localDeclarators
    , wipeTargets
    , producedLocals
    , outputs
    ) where

import           Data.Maybe                    (fromMaybe)
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Language.C.Analysis.SemRep    (FunType (..), Type (..),
                                                TypeDefRef (..), TypeName (..),
                                                constant, declType)
import           Language.C.Analysis.TravMonad (MonadTrav, lookupObject)
import           Language.C.Analysis.TypeUtils (canonicalType)
import           Language.C.Data.Ident         (Ident, identToString)
import           Language.C.Data.Node          (NodeInfo)
import           Language.C.Syntax.AST
import           Tokstyle.C.Analysis.Syntax    (exprChildren, localDecls,
                                                statExprs, stats)


-- | Functions that wipe the object their first argument points to.
wipers :: [String]
wipers = ["crypto_memzero", "sodium_memzero"]

-- | Functions that take a writable pointer but leave its contents as they are.
keepContents :: [String]
keepContents = ["crypto_memlock", "crypto_memunlock", "sodium_mlock", "sodium_munlock"]

-- | The local an argument of a wiper names, as in @x@ or @&x@.
wiped :: CExpr -> Maybe String
wiped = \case
    CVar i _          -> Just (identToString i)
    CUnary CAdrOp e _ -> wiped e
    CCast _ e _       -> wiped e
    _                 -> Nothing

-- | The expressions an expression evaluates, so not what @sizeof@ looks at.
subExprs :: CExpr -> [CExpr]
subExprs = \case
    CSizeofExpr{} -> []
    e             -> exprChildren e

-- | Uses the declared types, as the canonical type loses a @const@ on a
-- typedef such as @uint8_t@.
paramTypes :: MonadTrav m => CExpr -> m (Maybe [Type])
paramTypes = \case
    CVar i _ -> lookupObject i >>= \case
        Just d | FunctionType (FunType _ params _) _ <- declType d -> return (Just (map declType params))
        _ -> return Nothing
    _ -> return Nothing

pointee :: Type -> Maybe Type
pointee = \case
    PtrType t _ _                      -> Just t
    ArrayType t _ _ _                  -> Just t
    TypeDefType (TypeDefRef _ t _) _ _ -> pointee t
    _                                  -> Nothing

-- | Looks through typedefs itself, as 'typeQuals' loses a @const@ written on
-- one.
isConst :: Type -> Bool
isConst = \case
    DirectType _ q _                   -> constant q
    PtrType _ q _                      -> constant q
    ArrayType _ _ q _                  -> constant q
    TypeDefType (TypeDefRef _ t _) q _ -> constant q || isConst t
    FunctionType{}                     -> False

-- | Whether a call's argument at each position can be written through.
writableParams :: MonadTrav m => CExpr -> m (Int -> Bool)
writableParams f = paramTypes f >>= \case
    Just params ->
        let writable = map (maybe False (not . isConst) . pointee) params
        in return $ \n -> fromMaybe True (lookup n (zip [0 ..] writable))
    Nothing -> return (const True)

-- | Returns 'True' if the function reads bytes through a @const@ pointer, or
-- changes an object it's given, such as a stream it unpacks from. A function
-- that does neither makes its outputs itself, e.g. from randomness.
takesData :: MonadTrav m => CExpr -> m Bool
takesData f = maybe True (any (maybe False source . pointee)) <$> paramTypes f
  where
    source t = case canonicalType t of
        DirectType TyComp{} _ _ -> not (isConst t)
        _                       -> isConst t

declItems :: CDecl -> [(Ident, CDeclr, Maybe CInit)]
declItems = \case
    CDecl _ ds _ -> [ (i, d, ini) | (Just d@(CDeclr (Just i) _ _ _ _), ini, _) <- ds ]
    _            -> []

-- | The locals a body declares, with their declarators.
localDeclarators :: CStat -> [(Ident, CDeclr)]
localDeclarators body = [ (i, d) | decl <- localDecls body, (i, d, _) <- declItems decl ]

wipeTargets :: CExpr -> [String]
wipeTargets e = case e of
    CCall (CVar f _) (target : _) _ | identToString f `elem` wipers -> maybe [] pure (wiped target)
    _ -> concatMap wipeTargets (subExprs e)

-- | A function and the index of one of its arguments.
type Arg = (String, Int)

-- | Finds the locals written by calls to functions that take no data, as in
-- @f(x)@ or @f(&x)@.
producedLocals :: MonadTrav m => CStat -> m [(Arg, String, NodeInfo)]
producedLocals body = concat <$> mapM produced (concatMap calls (statExprs body))
  where
    locals = Set.fromList [ identToString i | (i, _) <- localDeclarators body ]
    calls e = [ (f, args, ni) | CCall f@(CVar callee _) args ni <- [e]
                              , identToString callee `notElem` wipers ++ keepContents ]
           ++ concatMap calls (subExprs e)
    produced (f@(CVar callee _), args, ni) = do
        writable <- writableParams f
        takesIn <- takesData f
        return [ ((identToString callee, n), v, ni)
               | not takesIn, (n, arg) <- zip [0 ..] args, writable n
               , Just v <- [wiped arg], v `Set.member` locals ]
    produced _ = return []

-- | Returns the arguments whose output a body wipes, and those whose output it
-- returns.
outputs :: MonadTrav m => CStat -> m (Set Arg, Set Arg)
outputs body = do
    produced <- producedLocals body
    let wipedLocals = Set.fromList (concatMap wipeTargets (statExprs body))
        returnedLocals = Set.fromList [ v | CReturn (Just e) _ <- stats body, Just v <- [wiped e] ]
        argsOf ls = Set.fromList [ arg | (arg, v, _) <- produced, v `Set.member` ls ]
    return (argsOf wipedLocals, argsOf returnedLocals)
