{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# language LambdaCase #-}

module CO4.Cache
  (MonadCache (..), CacheKey (..), Cache, runCache, withCache)
where

import           Control.Monad.State
import qualified Data.Map as M
import           Satchmo.Core.MonadSAT (MonadSAT (..))
import           CO4.EncodedAdt (EncodedAdt)

data CacheKey p = CacheCall String [EncodedAdt p]
                | CacheCase (EncodedAdt p) [EncodedAdt p]
                deriving (Eq,Ord)

class Monad m => MonadCache p m where
  retrieve :: CacheKey p -> m (Maybe (EncodedAdt p))
  cache    :: CacheKey p -> EncodedAdt p -> m ()

type CacheMap p = M.Map (CacheKey p) (EncodedAdt p)

data CacheState p = CacheState { cacheMap      :: CacheMap p
                               , numCallHits   :: Int
                               , numCallMisses :: Int
                               , numCaseHits   :: Int
                               , numCaseMisses :: Int
                               }

newtype Cache p m a = Cache { fromCache :: StateT (CacheState p) m a }
  deriving (Monad, MonadState (CacheState p), MonadTrans, MonadIO)

instance (Monad m, Ord p) => MonadCache p (Cache p m) where
  retrieve key = gets ( M.lookup key . cacheMap ) >>= \case
      Nothing -> addMiss >> return Nothing
      Just c  -> addHit  >> return ( Just c )
    where 
      addHit = case key of
        CacheCall {} -> modify $ \s -> s { numCallHits = succ $ numCallHits s }
        CacheCase {} -> modify $ \s -> s { numCaseHits = succ $ numCaseHits s }
      addMiss = case key of
        CacheCall {} -> modify $ \s -> s { numCallMisses = succ $ numCallMisses s }
        CacheCase {} -> modify $ \s -> s { numCaseMisses = succ $ numCaseMisses s }

  cache key result = modify $ \s -> s { cacheMap = M.insert key result $ cacheMap s }

instance (MonadSAT m) => MonadSAT (Cache p m) where
  fresh        = lift fresh
  emit         = lift . emit
  note         = lift . note
  numVariables = lift numVariables
  numClauses   = lift numClauses

runCache :: (MonadIO m) => Cache p m a -> m a
runCache c = do
  (result,cacheState) <- runStateT (fromCache c) $ CacheState M.empty 0 0 0 0

  let h1 = numCallHits   cacheState
      m1 = numCallMisses cacheState
      h2 = numCaseHits   cacheState
      m2 = numCaseMisses cacheState

  when ( h1 + m1 > 0 ) $ liftIO $ putStrLn $ 
    concat [ "Cache call hits: "  , show h1, " (", show $ (h1*100) `div` (h1+m1), "%), "
           ,       "call misses: ", show m1, " (", show $ (m1*100) `div` (h1+m1), "%)"
           ]
  when ( h2 + m2 > 0 ) $ liftIO $ putStrLn $ 
    concat [ "Cache case hits: "  , show h2, " (", show $ (h2*100) `div` (h2+m2), "%), "
           ,       "case misses: ", show m2, " (", show $ (m2*100) `div` (h2+m2), "%)"
           ]
  return result

withCache :: (MonadCache p m) => CacheKey p -> m (EncodedAdt p) -> m (EncodedAdt p)
withCache key f =
  retrieve key >>= \case 
    Just hit -> return hit
    Nothing  -> do
      result <- f
      cache key result
      return result
