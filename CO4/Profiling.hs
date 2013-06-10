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
import           Debug.Trace (trace)

class MonadSAT m => MonadProfiling m where
  traced :: String -> m a -> m a

data ProfileData = ProfileData { numCalls     :: Int
                               , numVariables :: Int
                               , numClauses   :: Int
                               } 
                               deriving Show

type ProfileMap = M.Map String ProfileData

data Profile = Profile { innerUnder      :: ProfileMap
                       , inner           :: ProfileMap
                       , currentFunction :: String
                       , stackDepth      :: Int
                       }

onInnerUnder,onInner :: (ProfileMap -> ProfileMap) -> Profile -> Profile
onInnerUnder f p = p { innerUnder = f $ innerUnder p }
onInner      f p = p { inner      = f $ inner      p }

onStackDepth :: (Int -> Int) -> Profile -> Profile
onStackDepth f p = p { stackDepth = f $ stackDepth p }

setCurrentFunction :: String -> Profile -> Profile
setCurrentFunction name p = p { currentFunction = name }

newtype SimpleProfiling m a = SimpleProfiling 
  { fromSimpleProfiling :: StateT Profile m a }
  deriving (Monad, MonadState Profile, MonadTrans)

instance MonadSAT m => MonadSAT (SimpleProfiling m) where
  fresh = do
    c <- gets currentFunction
    modify $ onInner $ 
      M.alter (\case Nothing -> Nothing 
                     Just p  -> Just $ p { numVariables = succ $ numVariables p }
              ) c
    lift MonadSAT.fresh

  emit clause = do 
    c <- gets currentFunction
    modify $ onInner $
      M.alter (\case Nothing -> Nothing
                     Just p  -> Just $ p { numClauses = succ $ numClauses p }
              ) c
    lift $ MonadSAT.emit clause

  note         = lift . MonadSAT.note
  numVariables = lift   MonadSAT.numVariables
  numClauses   = lift   MonadSAT.numClauses

instance MonadSAT m => MonadProfiling (SimpleProfiling m) where
  traced name action = do
    previous <- gets currentFunction
    modify $ setCurrentFunction name

    modify $ onInner $ M.alter 
      (\case Nothing -> Just $ ProfileData 1 0 0
             Just p  -> Just $ p { numCalls = succ $ numCalls p }) name

    sd     <- gets stackDepth
    modify $ onStackDepth succ

    v1     <- MonadSAT.numVariables
    c1     <- MonadSAT.numClauses
    result <- trace (replicate (2*sd) ' ' ++ "Running '" ++ name ++ "'") action
    v2     <- MonadSAT.numVariables
    c2     <- MonadSAT.numClauses

    modify $ setCurrentFunction previous

    modify $ onInnerUnder $ M.alter
      ( \case Nothing -> Just $ ProfileData 1 (v2 - v1) (c2 - c1)
              Just p  -> Just $ p { numCalls     = numCalls     p + 1
                                  , numVariables = numVariables p + (v2 - v1)
                                  , numClauses   = numClauses   p + (c2 - c1)
                                  }
      ) name 

    modify $ onStackDepth pred

    return result

instance MonadCache p m => MonadCache p (SimpleProfiling m) where
  retrieve = lift . retrieve
  cache k  = lift . cache k

simpleProfiling :: (MonadIO m, MonadSAT m) => SimpleProfiling m a -> m a
simpleProfiling p = do 
  (result, profile) <- runStateT (fromSimpleProfiling p) 
                                 (Profile M.empty M.empty "__init" 0)
  printProfile "inner-under" innerUnder profile
  printProfile "inner"       inner      profile
  return result
  where
    printProfile msg p profile =
      when (not $ M.null $ p profile) $
        liftIO $ putStrLn $ unlines 
               $ (concat ["Profiling (",msg,"): "] :) 
               $ map show 
               $ reverse
               $ sortBy (compare `on` (numVariables . snd))
               $ M.toList $ p profile
