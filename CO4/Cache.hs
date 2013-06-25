{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# language LambdaCase #-}

module CO4.Cache
  (MonadCache (..), CacheKey (..), Cache, runCache, withCache)
where

import           Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import           Satchmo.Core.MonadSAT (MonadSAT (..))
import           CO4.EncodedAdt (EncodedAdt(..),isValid,constantConstructorIndex)

data CacheKey e = CacheCall String [e]
                | CacheCase e [e]
                deriving (Eq,Ord)

class Monad m => MonadCache e m where
  retrieve :: CacheKey e -> m (Maybe e)
  cache    :: CacheKey e -> e -> m ()

type CacheMap e = M.Map (CacheKey e) e

data CacheState e = CacheState { cacheMap      :: CacheMap e
                               , numCallHits   :: Int
                               , numCallMisses :: Int
                               , numCaseHits   :: Int
                               , numCaseMisses :: Int
                               }

newtype Cache e m a = Cache { fromCache :: StateT (CacheState e) m a }
  deriving (Monad, MonadState (CacheState e), MonadTrans, MonadIO)

instance (Monad m, Ord e) => MonadCache e (Cache e m) where
  retrieve key = gets ( M.lookup key . cacheMap ) >>= \case
      Nothing -> addMiss >> return Nothing
      Just e  -> addHit  >> return ( Just e )
    where 
      addHit = case key of
        CacheCall {} -> modify $ \s -> s { numCallHits = succ $ numCallHits s }
        CacheCase {} -> modify $ \s -> s { numCaseHits = succ $ numCaseHits s }
      addMiss = case key of
        CacheCall {} -> modify $ \s -> s { numCallMisses = succ $ numCallMisses s }
        CacheCase {} -> modify $ \s -> s { numCaseMisses = succ $ numCaseMisses s }

  cache key result = modify $ \s -> s { cacheMap = M.insert key result $ cacheMap s }

instance (MonadSAT m) => MonadSAT (Cache e m) where
  fresh        = lift fresh
  emit         = lift . emit
  note         = lift . note
  numVariables = lift numVariables
  numClauses   = lift numClauses

runCache :: (MonadIO m) => Cache e m a -> m a
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

withCache :: (EncodedAdt e p, MonadCache (e p) m) 
          => CacheKey (e p) -> m (e p) -> m (e p)
withCache key f = case key of
  CacheCase e _ -> if isValid e
                   then case constantConstructorIndex e of
                         Just _ -> f
                         _      -> askCache
                   else f
  _ -> askCache
  where
    askCache = retrieve key >>= \case 
                  Just hit -> return hit
                  Nothing  -> do
                    result <- f
                    cache key result
                    return result
