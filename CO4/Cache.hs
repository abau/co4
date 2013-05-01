{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# language LambdaCase #-}

module CO4.Cache
  (MonadCache (..), Cache, runCache, withCache)
where

import           Control.Monad.State
import qualified Data.Map as M
import           Satchmo.Core.MonadSAT (MonadSAT (..))
import           CO4.EncodedAdt (EncodedAdt)

class Monad m => MonadCache p m where
  retrieve :: String -> [EncodedAdt p] -> m (Maybe (EncodedAdt p))
  cache    :: String -> [EncodedAdt p] -> EncodedAdt p -> m ()

type CacheMap p = M.Map (String,[EncodedAdt p]) (EncodedAdt p)

data CacheState p = CacheState { cacheMap  :: CacheMap p
                               , numHits   :: Int
                               , numMisses :: Int
                               }

newtype Cache p m a = Cache { fromCache :: StateT (CacheState p) m a }
  deriving (Monad, MonadState (CacheState p), MonadTrans, MonadIO)

instance (Monad m, Ord p) => MonadCache p (Cache p m) where
  retrieve fun args = gets ( M.lookup (fun,args) . cacheMap ) >>= \case
      Nothing -> addMiss >> return Nothing
      Just c  -> addHit  >> return ( Just c )
    where 
      addHit  = modify $ \s -> s { numHits   = succ $ numHits   s }
      addMiss = modify $ \s -> s { numMisses = succ $ numMisses s }

  cache    fun args result = 
    modify $ \s -> s { cacheMap = M.insert (fun,args) result $ cacheMap s }

instance (MonadSAT m) => MonadSAT (Cache p m) where
  fresh        = lift fresh
  emit         = lift . emit
  note         = lift . note
  numVariables = lift numVariables
  numClauses   = lift numClauses

runCache :: (MonadIO m) => Cache p m a -> m a
runCache c = do
  (result,cacheState) <- runStateT (fromCache c) $ CacheState M.empty 0 0

  let h = numHits   cacheState
      m = numMisses cacheState

  when ( h + m > 0 ) $ liftIO $ putStrLn $ 
    concat [ "Cache hits: "  , show h, " (", show $ (h*100) `div` (h+m), "%), "
           ,       "misses: ", show m, " (", show $ (m*100) `div` (h+m), "%)"
           ]
  return result

withCache :: (MonadCache p m) => String -> [EncodedAdt p] -> m (EncodedAdt p)
                              -> m (EncodedAdt p)
withCache name args f =
  retrieve name args >>= \case 
    Just hit -> return hit
    Nothing  -> do
      result <- f
      cache name args result
      return result
