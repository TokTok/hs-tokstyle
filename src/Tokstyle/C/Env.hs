{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Strict         #-}
module Tokstyle.C.Env where

import           Language.C.Analysis.SemRep    (Type)
import           Language.C.Analysis.TravMonad (Trav, getUserState,
                                                modifyUserState)
import           Language.C.Data.Ident         (Ident)
import           Tokstyle.C.TravUtils          (getJust)

data Env = Env
    { ctx    :: [String]
    , retTy  :: Maybe Type
    , params :: [Ident]
    }

defaultEnv :: Env
defaultEnv = Env ["file"] Nothing []

bracketUserState :: (s -> s) -> Trav s a -> Trav s a
bracketUserState f act = do
    s <- getUserState
    modifyUserState f
    r <- act
    modifyUserState (const s)
    return r


getCtx :: Trav Env [String]
getCtx = ctx <$> getUserState

pushCtx :: String -> Trav Env ()
pushCtx s = modifyUserState $ \env@Env{ctx} -> env{ctx = s:ctx}

popCtx :: Trav Env ()
popCtx = modifyUserState $ \env@Env{ctx} -> env{ctx = tail ctx}


getRetTy :: Trav Env Type
getRetTy = getUserState >>= getJust "not in function context" . retTy

setRetTy :: Type -> Trav Env ()
setRetTy t = modifyUserState $ \env -> env{retTy = Just t}

unsetRetTy :: Trav Env ()
unsetRetTy = modifyUserState $ \env -> env{retTy = Nothing}
