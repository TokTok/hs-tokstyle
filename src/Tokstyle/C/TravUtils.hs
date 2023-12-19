module Tokstyle.C.TravUtils where

import           Language.C.Analysis.TravMonad (MonadTrav, throwTravError)
import           Language.C.Data.Error         (userErr)


getJust :: MonadTrav m => String -> Maybe a -> m a
getJust _ (Just ok) = return ok
getJust failMsg Nothing =
    throwTravError $ userErr failMsg
