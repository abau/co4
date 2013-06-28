{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module CO4.Cache
  (MonadCache (..), CacheKey, Cache, runCache, withCache)
where

import           Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import           Satchmo.Core.MonadSAT (MonadSAT (..))

type CacheKey e = (String,[e])

class Monad m => MonadCache e m where
  retrieve :: CacheKey e -> m (Maybe e)
  cache    :: CacheKey e -> e -> m ()

type CacheMap e = M.Map (CacheKey e) e

data CacheState e = CacheState { cacheMap  :: CacheMap e
                               , numHits   :: Int
                               , numMisses :: Int
                               }

newtype Cache e m a = Cache { fromCache :: StateT (CacheState e) m a }
  deriving (Monad, MonadState (CacheState e), MonadTrans, MonadIO)

instance (Monad m, Ord e) => MonadCache e (Cache e m) where
  retrieve key = gets ( M.lookup key . cacheMap ) >>= \case
      Nothing -> addMiss >> return Nothing
      Just e  -> addHit  >> return ( Just e )
    where 
      addHit  = modify $ \s -> s { numHits   = succ $ numHits   s }
      addMiss = modify $ \s -> s { numMisses = succ $ numMisses s }

  cache key result = modify $ \s -> s { cacheMap = M.insert key result $ cacheMap s }

instance (MonadSAT m) => MonadSAT (Cache e m) where
  fresh        = lift fresh
  emit         = lift . emit
  note         = lift . note
  numVariables = lift numVariables
  numClauses   = lift numClauses

runCache :: (MonadIO m) => Cache e m a -> m a
runCache c = do
  (result,cacheState) <- runStateT (fromCache c) $ CacheState M.empty 0 0

  let h = numHits   cacheState
      m = numMisses cacheState

  when ( h + m > 0 ) $ liftIO $ putStrLn $ 
    concat [ "Cache hits: ", show h, " (", show $ (h*100) `div` (h+m), "%)"]

  return result

withCache :: (MonadCache (e p) m) => CacheKey (e p) -> m (e p) -> m (e p)
withCache key f = retrieve key >>= \case 
  Just hit -> return hit
  Nothing  -> do
    result <- f
    cache key result
    return result
