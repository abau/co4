{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}

module CO4.Cache
  ( MonadCache (..), Cache, runCache, withCache
  , MonadCallCache, CallCache 
  )
where

import           Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import           Satchmo.Core.MonadSAT (MonadSAT (..))

class Monad m => MonadCache k v m where
  retrieve :: k -> m (Maybe v)
  cache    :: k -> v -> m ()

data CacheState k v = CacheState { cacheMap  :: M.Map k v
                                 , numHits   :: Int
                                 , numMisses :: Int
                                 }

newtype Cache k v m a = Cache { fromCache :: StateT (CacheState k v) m a }
  deriving (Monad, MonadState (CacheState k v), MonadTrans, MonadIO)

instance (Monad m, Ord k) => MonadCache k v (Cache k v m) where
  retrieve key = gets ( M.lookup key . cacheMap ) >>= \case
      Nothing -> addMiss >> return Nothing
      Just e  -> addHit  >> return ( Just e )
    where 
      addHit  = modify $ \s -> s { numHits   = succ $ numHits   s }
      addMiss = modify $ \s -> s { numMisses = succ $ numMisses s }

  cache key result = modify $ \s -> s { cacheMap = M.insert key result $ cacheMap s }

instance (MonadSAT m) => MonadSAT (Cache k v m) where
  fresh        = lift fresh
  emit         = lift . emit
  note         = lift . note
  numVariables = lift numVariables
  numClauses   = lift numClauses

runCache :: (MonadIO m) => Cache k v m a -> m a
runCache c = do
  (result,cacheState) <- runStateT (fromCache c) $ CacheState M.empty 0 0

  let h = numHits   cacheState
      m = numMisses cacheState

  when ( h + m > 0 ) $ liftIO $ putStrLn $ 
    concat [ "Cache hits: ", show h, " (", show $ (h*100) `div` (h+m), "%), "
           ,     "misses: ", show m, " (", show $ (m*100) `div` (h+m), "%)" ]

  return result

withCache :: (MonadCache k v m) => k -> m v -> m v
withCache key f = retrieve key >>= \case 
  Just hit -> return hit
  Nothing  -> do
    result <- f
    cache key result
    return result

-- * Type aliases for special caches

type MonadCallCache e = MonadCache (String,[e]) e
type CallCache e      = Cache (String,[e]) e

