{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module CO4.Profiling 
  (MonadProfiling (..), SimpleProfiling, simpleProfiling)
where

import           Control.Monad.State
import           Data.List (sortBy)
import           Data.Function (on)
import qualified Data.Map as M
import           Satchmo.Core.MonadSAT (MonadSAT)
import qualified Satchmo.Core.MonadSAT as MonadSAT
import           CO4.Cache (MonadCache (..))

class MonadSAT m => MonadProfiling m where
  traced :: String -> m a -> m a

data ProfileData = ProfileData { numCalls     :: Int
                               , numVariables :: Int
                               , numClauses   :: Int
                               } 
                               deriving Show

type Profile = M.Map String ProfileData

newtype SimpleProfiling m a = SimpleProfiling 
  { fromSimpleProfiling :: StateT Profile m a }
  deriving (Monad, MonadState Profile, MonadTrans)

instance MonadSAT m => MonadSAT (SimpleProfiling m) where
  fresh        = lift   MonadSAT.fresh
  emit         = lift . MonadSAT.emit
  note         = lift . MonadSAT.note
  numVariables = lift   MonadSAT.numVariables
  numClauses   = lift   MonadSAT.numClauses

instance MonadSAT m => MonadProfiling (SimpleProfiling m) where
  traced name action = do
    v1     <- MonadSAT.numVariables
    c1     <- MonadSAT.numClauses
    result <- action
    v2     <- MonadSAT.numVariables
    c2     <- MonadSAT.numClauses

    modify $ M.alter
      ( \case Nothing -> Just $ ProfileData 1 (v2 - v1) (c2 - c1)
              Just p  -> Just $ p { numCalls     = numCalls     p + 1
                                  , numVariables = numVariables p + (v2 - v1)
                                  , numClauses   = numClauses   p + (c2 - c1)
                                  }
      ) name
    return result

instance MonadCache p m => MonadCache p (SimpleProfiling m) where
  retrieve s   = lift . retrieve s
  cache    s a = lift . cache s a

simpleProfiling :: (MonadIO m, MonadSAT m) => SimpleProfiling m a -> m a
simpleProfiling p = do 
  (result, profile) <- runStateT (fromSimpleProfiling p) M.empty
  when (not $ M.null profile) $
    liftIO $ putStrLn $ unlines 
           $ ("Profiling:" :) 
           $ map show 
           $ reverse
           $ sortBy (compare `on` (numVariables . snd))
           $ M.toList profile
  return result
