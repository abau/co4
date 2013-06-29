{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module CO4.Profiling 
  (MonadProfiling (..), SimpleProfiling, simpleProfiling)
where

import           Control.Monad.State.Strict
import           Data.List (sortBy)
import           Data.Function (on)
import qualified Data.Map.Strict as M
import           Satchmo.Core.MonadSAT (MonadSAT)
import qualified Satchmo.Core.MonadSAT as MonadSAT
import           CO4.Cache (MonadCache (..))
--import           Debug.Trace (trace)

class MonadSAT m => MonadProfiling m where
  traced :: String -> m a -> m a

data ProfileData = ProfileData { numCalls     :: ! Int
                               , numVariables :: ! Int
                               , numClauses   :: ! Int
                               } 
                               deriving Show

nullProfileData :: ProfileData
nullProfileData = ProfileData 0 0 0

type ProfileMap = M.Map String ProfileData

data Profile = Profile { innerUnder      :: ! ProfileMap
                       , inner           :: ! ProfileMap
                       , currentFunction :: ! String
                       , currentInner    :: ! ProfileData
                       , stackDepth      :: ! Int
                       }

nullProfile :: Profile
nullProfile = Profile M.empty M.empty "__init" nullProfileData 0

onInnerUnder,onInner :: (ProfileMap -> ProfileMap) -> Profile -> Profile
onInnerUnder   f p = p { innerUnder   = f $ innerUnder   p }
onInner        f p = p { inner        = f $ inner        p }

onCurrentInner :: (ProfileData -> ProfileData) -> Profile -> Profile
onCurrentInner f p = p { currentInner = f $ currentInner p }

onStackDepth :: (Int -> Int) -> Profile -> Profile
onStackDepth f p = p { stackDepth = f $ stackDepth p }

setCurrentFunction :: String -> Profile -> Profile
setCurrentFunction name p = p { currentFunction = name }

newtype SimpleProfiling m a = SimpleProfiling 
  { fromSimpleProfiling :: StateT Profile m a }
  deriving (Monad, MonadState Profile, MonadTrans)

instance MonadSAT m => MonadSAT (SimpleProfiling m) where
  fresh = do
    modify $! onCurrentInner $! \p -> p { numVariables = succ $ numVariables p }
    lift MonadSAT.fresh

  emit clause = do 
    modify $! onCurrentInner $! \p -> p { numClauses = succ $ numClauses p }
    lift $ MonadSAT.emit clause

  note         = lift . MonadSAT.note
  numVariables = lift   MonadSAT.numVariables
  numClauses   = lift   MonadSAT.numClauses

instance MonadSAT m => MonadProfiling (SimpleProfiling m) where
  traced name action = do
    writeCurrentInner
    previous <- gets currentFunction

    modify $! setCurrentFunction name

    modify $! onInner $! M.alter 
      (\case Nothing -> Just $ ProfileData 1 0 0
             Just p  -> Just $ p { numCalls = succ $ numCalls p }) name

    --sd     <- gets stackDepth
    modify $! onStackDepth succ

    v1     <- MonadSAT.numVariables
    c1     <- MonadSAT.numClauses
    result <- {-trace (replicate (2*sd) ' ' ++ "Running '" ++ name ++ "'")-} action
    v2     <- MonadSAT.numVariables
    c2     <- MonadSAT.numClauses

    writeCurrentInner
    modify $! setCurrentFunction previous
    modify $! onInnerUnder $ M.alter
      ( \case Nothing -> Just $ ProfileData 1 (v2 - v1) (c2 - c1)
              Just p  -> Just $ p { numCalls     = numCalls     p + 1
                                  , numVariables = numVariables p + (v2 - v1)
                                  , numClauses   = numClauses   p + (c2 - c1)
                                  }
      ) name 

    modify $! onStackDepth pred
    return result

    where
      writeCurrentInner = do
        fun   <- gets currentFunction
        pData <- gets currentInner

        modify $! onInner $! M.alter
          ( \case Nothing -> Just pData 
                  Just p  -> Just $! 
                    p { numCalls     = numCalls     p + numCalls     pData
                      , numVariables = numVariables p + numVariables pData
                      , numClauses   = numClauses   p + numClauses   pData
                      }
          ) fun
        modify $! onCurrentInner $! const nullProfileData

instance MonadCache k v m => MonadCache k v (SimpleProfiling m) where
  retrieve = lift . retrieve
  cache k  = lift . cache k

simpleProfiling :: (MonadIO m, MonadSAT m) => SimpleProfiling m a -> m a
simpleProfiling p = do 
  (result, profile) <- runStateT (fromSimpleProfiling p) nullProfile
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
