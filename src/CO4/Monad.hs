{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module CO4.Monad
  ( CO4, SAT, newId, getCallStackTrace, isProfileRun, setProfileRun, abortWithStackTrace
  , runCO4, withCallCache, withAdtCache, traced
  )
where

import           Control.Monad.State.Strict
import           Control.Applicative (Applicative)
import           Data.List (nub)
import qualified Satchmo.Core.SAT.Minisat 
import           Satchmo.Core.MonadSAT (MonadSAT (..))
import qualified Satchmo.Core.MonadSAT as MonadSAT
import           CO4.Cache 
import           CO4.Profiling as P
import           CO4.Stack
import {-# SOURCE #-} CO4.EncodedAdt (Primitive,EncodedAdt,makeWithId)

type SAT          = Satchmo.Core.SAT.Minisat.SAT
type AdtCacheKey  = (Primitive, [Primitive], [EncodedAdt], Bool)
type CallCacheKey = (String, [EncodedAdt])
type AdtCache     = Cache AdtCacheKey  Int
type CallCache    = Cache CallCacheKey EncodedAdt

data CO4Data = CO4Data { 
    idCounter  :: ! Int
  , adtCache   :: ! AdtCache
  , callCache  :: ! CallCache
  , profile    :: ! Profile
  , callStack  :: ! CallStack
  , profileRun :: ! Bool
  }

emptyData :: CO4Data
emptyData = CO4Data 0 emptyCache emptyCache emptyProfile emptyStack False

newtype CO4 a = CO4 { unCO4 :: StateT CO4Data SAT a }
  deriving (Functor, Applicative, Monad, MonadState CO4Data)

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

onCallStack :: (CallStack -> CallStack) -> CO4Data -> CO4Data
onCallStack f c = c { callStack = f $ callStack c }

getCallStackTrace :: CO4 CallStackTrace
getCallStackTrace = gets $ trace . callStack

isProfileRun :: CO4 Bool
isProfileRun = gets profileRun

setProfileRun :: CO4 ()
setProfileRun = modify $! \c -> c { profileRun = True }

abortWithStackTrace :: String -> CO4 a
abortWithStackTrace msg = do
  stackTrace <- getCallStackTrace

  let trace = if null stackTrace 
              then format ("stack trace", "no stack trace available")
              else format ("stack trace", unlines stackTrace)

  error $ unlines [ msg, trace ]
  where
    format (header,trace) = 
      concat ["## ", header, " ", replicate (20 - length header) '#', "\n"] 
      ++ trace

instance MonadSAT CO4 where
  fresh = do
    modify $! onProfile $! onCurrentInner incNumVariables
    liftSAT fresh 

  emit c = do
    modify $! onProfile $! onCurrentInner incNumClauses
    liftSAT $! emit c

  note         = liftSAT . note
  numVariables = liftSAT MonadSAT.numVariables
  numClauses   = liftSAT MonadSAT.numClauses

runCO4 :: CO4 a -> SAT a
runCO4 p = do
  (result, co4Data) <- runStateT (unCO4 p) emptyData
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
withAdtCache key@(d,fs,args,pf) = gets (retrieve key . adtCache) >>= \case
  (Just id, c) -> do modify (setAdtCache c) 
                     return $ makeWithId id d fs args pf
  (Nothing, c) -> do 
    id     <- newId
    modify $! setAdtCache $! cache key id c
    return $  makeWithId id d fs args pf

traced :: String -> CO4 a -> CO4 a
traced name action = do
  setProfileRun
  previousFun   <- gets $ currentFunction . profile
  previousInner <- gets $ currentInner    . profile

  modify $! onProfile   $! callFunction name . resetCurrentInner
  modify $! onCallStack $! pushToStack name

  result <- action

  vs     <- gets $ P.numVariables . currentInner . profile
  cs     <- gets $ P.numClauses   . currentInner . profile
  trace' <- getCallStackTrace >>= return . nub

  modify $! onProfile $ \p -> foldr (incInnerUnderBy vs cs) p trace'
  modify $! onProfile $ returnTo previousFun previousInner

  modify $! onCallStack $! popFromStack
  return result
