{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# language LambdaCase #-}

module CO4.Cache
  (MonadCache, Cache, runCache, withCache)
where

import           Control.Monad.State
import qualified Data.Map as M
import           Satchmo.Core.MonadSAT (MonadSAT (..))
import           CO4.EncodedAdt (EncodedAdt)

class Monad m => MonadCache p m where
  retrieve :: String -> [EncodedAdt p] -> m (Maybe (EncodedAdt p))
  cache    :: String -> [EncodedAdt p] -> EncodedAdt p -> m ()

type CacheMap p = M.Map (String,[EncodedAdt p]) (EncodedAdt p)

newtype Cache p m a = Cache { fromCache :: StateT (CacheMap p) m a }
  deriving (Monad, MonadState (CacheMap p), MonadTrans)

instance (Monad m, Ord p) => MonadCache p (Cache p m) where
  retrieve fun args        = gets   $ M.lookup (fun,args)
  cache    fun args result = modify $ M.insert (fun,args) result

instance (MonadSAT m) => MonadSAT (Cache p m) where
  fresh        = lift fresh
  emit         = lift . emit
  note         = lift . note
  numVariables = lift numVariables
  numClauses   = lift numClauses

runCache :: (Monad m) => Cache p m a -> m a
runCache c = evalStateT (fromCache c) M.empty

withCache :: (MonadCache p m) => String -> [EncodedAdt p] -> m (EncodedAdt p)
                              -> m (EncodedAdt p)
withCache name args f =
  retrieve name args >>= \case 
    Just hit -> return hit
    Nothing  -> do
      result <- f
      cache name args result
      return result

{-
newtype NoCache m a = NoCache { fromNoCache :: m a }
  deriving (Monad)

instance (Monad m) => MonadCache p (NoCache m) where
  retrieve _ _ = return Nothing
  cache _ _ _  = return ()

instance (MonadSAT m) => MonadSAT (NoCache m) where
  fresh = NoCache fresh
  emit  = NoCache . emit
  note  = NoCache . note

noCache :: NoCache m a -> m a
noCache = fromNoCache
-}

