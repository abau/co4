{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module CO4.Monad
  (CO4, SAT, newId, runCO4, withCallCache, withAdtCache, traced)
where

import           Control.Monad.State.Strict
import qualified Satchmo.Core.SAT.Minisat 
import           Satchmo.Core.MonadSAT (MonadSAT (..))
import           CO4.EncodedAdtData (Primitive,EncodedAdt(..))
import           CO4.Cache 
import           CO4.Profiling

type SAT          = Satchmo.Core.SAT.Minisat.SAT
type AdtCacheKey  = (Primitive, [Primitive], [EncodedAdt])
type CallCacheKey = (String, [EncodedAdt])
type AdtCache     = Cache AdtCacheKey  Int
type CallCache    = Cache CallCacheKey EncodedAdt

data CO4Data = CO4Data { 
    idCounter :: ! Int
  , adtCache  :: ! AdtCache
  , callCache :: ! CallCache
  , profile   :: ! Profile
  }

newtype CO4 a = CO4 { unCO4 :: StateT CO4Data SAT a }
  deriving (Monad, MonadState CO4Data)

liftSAT :: SAT a -> CO4 a
liftSAT sat = CO4 $! StateT $! \s -> sat >>= \r -> return (r,s)

newId :: CO4 Int
newId = do  
  id <- gets idCounter
  modify $! \s -> s { idCounter = id + 1 }
  return id

onProfile :: (Profile -> Profile) -> CO4Data -> CO4Data
onProfile f c = c { profile = f $ profile c }

onAdtCache :: (AdtCache -> AdtCache) -> CO4Data -> CO4Data
onAdtCache f c = c { adtCache = f $ adtCache c }

setAdtCache :: AdtCache -> CO4Data -> CO4Data
setAdtCache = onAdtCache . const

onCallCache :: (CallCache -> CallCache) -> CO4Data -> CO4Data
onCallCache f c = c { callCache = f $ callCache c }

setCallCache :: CallCache -> CO4Data -> CO4Data
setCallCache = onCallCache . const

instance MonadSAT CO4 where
  fresh = do
    modify $! onProfile $! onCurrentInner incNumVariables
    liftSAT fresh 

  emit c = do
    modify $! onProfile $! onCurrentInner incNumClauses
    liftSAT $! emit c

  note         = liftSAT . note
  numVariables = liftSAT numVariables
  numClauses   = liftSAT numClauses

runCO4 :: CO4 a -> SAT a
runCO4 p = do
  (result, co4Data) <- runStateT (unCO4 p) $ CO4Data 0 emptyCache emptyCache 
                                                     emptyProfile
  let h = numHits   $ callCache co4Data
      m = numMisses $ callCache co4Data

  when ( h + m > 0 ) $ note $ 
    concat [ "Cache hits: ", show h, " (", show $ (h*100) `div` (h+m), "%), "
           ,     "misses: ", show m, " (", show $ (m*100) `div` (h+m), "%)" ]
  printProfile $ profile co4Data
  return result

withCallCache :: CallCacheKey -> CO4 EncodedAdt -> CO4 EncodedAdt
withCallCache key action = 
 gets (retrieve key . callCache) >>= \case
  (Just hit, c) -> modify (setCallCache c) >> return hit
  (Nothing , c) -> do
    modify $! setCallCache c
    result <- action 
    modify $! onCallCache $! cache key result 
    return result

withAdtCache :: AdtCacheKey -> CO4 EncodedAdt
withAdtCache key@(d,fs,args) = gets (retrieve key . adtCache) >>= \case
  (Just id, c) -> modify (setAdtCache c) >> return (EncodedAdt id d fs args)
  (Nothing, c) -> do 
    id <- newId
    modify $! setAdtCache $! cache key id c
    return $ EncodedAdt id d fs args

traced :: String -> CO4 a -> CO4 a
traced name action = do
  previous <- gets $ currentFunction . profile
                     
  modify $! onProfile ( setCurrentFunction name 
                      . onCurrentInner incNumCalls
                      . writeCurrentInner )

  v1     <- numVariables
  c1     <- numClauses
  result <- action
  v2     <- numVariables
  c2     <- numClauses

  modify $! onProfile ( incInnerUnderBy 1 (v2 - v1) (c2 - c1) name
                      . setCurrentFunction previous 
                      . writeCurrentInner )
  return result
