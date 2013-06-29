{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CO4.Algorithms.Eitherize.Solve
   ( ConstraintSystem, ParamConstraintSystem
   , booleanCache, profile
   , solveAndTestBooleanP, solveBooleanP
   , solveAndTestBoolean, solveBoolean
   , solveAndTestP, solveP
   , solveAndTest, solve
   )
where

import           Prelude hiding (and)
import           Control.Monad.IO.Class (MonadIO)
import           System.IO (hFlush,stdout)
import           Satchmo.Core.MonadSAT (MonadSAT,note)
import qualified Satchmo.Core.SAT.Minisat as Backend 
import           Satchmo.Core.Decode (Decode,decode)
import           Satchmo.Core.Primitive (Primitive,assert,and)
import           Satchmo.Core.Boolean (Boolean)
import qualified CO4.EncodedAdt as E
import           CO4.Allocator.Common (Allocator)
import           CO4.Cache (CallCache,runCache)
import           CO4.Encodeable (Encodeable (..))
import           CO4.Profiling (SimpleProfiling, simpleProfiling)

import           CO4.Allocator.Overlapping ()
import           CO4.EncodedAdt.Overlapping (Overlapping)

type EncodedAdt  = Overlapping

type ConstraintSystem m p      = (EncodedAdt p) -> m (EncodedAdt p)
type ParamConstraintSystem m p = (EncodedAdt p) -> ConstraintSystem m p

booleanCache :: (MonadIO m) => CallCache (EncodedAdt Boolean) m a -> m a
booleanCache = runCache

profile :: (MonadIO m, MonadSAT m) => SimpleProfiling m a -> m a
profile = simpleProfiling

solveAndTestBooleanP :: ( Show a, Show b, Decode Backend.SAT (EncodedAdt Boolean) a
                        , Monad m, Encodeable k EncodedAdt Boolean ) 
                     => SolveAndTestP k m Boolean a b
solveAndTestBooleanP = solveAndTestP

solveBooleanP :: ( Decode Backend.SAT (EncodedAdt Boolean) a, Monad m
                 , Encodeable k EncodedAdt Boolean ) => SolveP k m Boolean a
solveBooleanP = solveP

solveAndTestBoolean :: ( Show a, Show b, Decode Backend.SAT (EncodedAdt Boolean) a
                       , Monad m) => SolveAndTest m Boolean a b
solveAndTestBoolean = solveAndTest

solveBoolean :: (Decode Backend.SAT (EncodedAdt Boolean) a, Monad m) 
             => Solve m Boolean a
solveBoolean = solve

type SolveAndTestP k m p a b =
     k                                                -- ^Known parameter
  -> (m (EncodedAdt p) -> Backend.SAT (EncodedAdt p)) -- ^Mapping to non-monadic constraint system
  -> Allocator                                        -- ^Allocator
  -> ParamConstraintSystem m p                        -- ^Encoded constraint system
  -> (k -> a -> b)                                    -- ^Original constraint system
  -> IO (Maybe a)

-- |Solves an encoded, parametrized, monadic constraint system and tests the 
-- found solution against the original constraint system
solveAndTestP :: ( Primitive p, Show a, Show b, Decode Backend.SAT (EncodedAdt p) a
                 , Monad m, Encodeable k EncodedAdt p ) 
              => SolveAndTestP k m p a b
solveAndTestP k f allocator constraint test =
  solve f allocator (constraint $ encodeConstant k) >>= testSolution (test k)

type SolveP k m p a =
     k                                                -- ^Known parameter
  -> (m (EncodedAdt p) -> Backend.SAT (EncodedAdt p)) -- ^Mapping to non-monadic constraint system
  -> Allocator                                        -- ^Allocator
  -> ParamConstraintSystem m p                        -- ^Encoded constraint system
  -> IO (Maybe a)

-- |Solves an encoded, parametrized, monadic constraint system
solveP :: (Primitive p, Decode Backend.SAT (EncodedAdt p) a, Monad m
          , Encodeable k EncodedAdt p )
       => SolveP k m p a
solveP k f allocator constraint = solve f allocator (constraint $ encodeConstant k)

type SolveAndTest m p a b = 
     (m (EncodedAdt p) -> Backend.SAT (EncodedAdt p)) -- ^Mapping to non-monadic constraint system
  -> Allocator                                        -- ^Allocator
  -> (ConstraintSystem m p)                           -- ^Encoded monadic constraint system
  -> (a -> b)                                         -- ^Original constraint system
  -> IO (Maybe a)

-- |Solves an encoded, monadic constraint system and tests the found solution
-- against the original constraint system
solveAndTest :: ( Primitive p, Show a, Show b, Decode Backend.SAT (EncodedAdt p) a
                 , Monad m ) => SolveAndTest m p a b
solveAndTest f allocator constraint test = 
  solve f allocator constraint >>= testSolution test

type Solve m p a = 
     (m (EncodedAdt p) -> Backend.SAT (EncodedAdt p)) -- ^Mapping to non-monadic constraint system
  -> Allocator                                        -- ^Allocator
  -> (ConstraintSystem m p)                           -- ^Encoded monadic constraint system
  -> IO (Maybe a)

-- |Solves an encoded, monadic constraint system
solve :: (Primitive p, Decode Backend.SAT (EncodedAdt p) a, Monad m) => Solve m p a
solve f allocator constraint = 
  Backend.solve' True $ do 
    unknown <- encode allocator
    --Backend.note $ "Encoded unknown:\n" ++ show unknown
    result <- f $ constraint unknown
    handleResult unknown result

testSolution :: (Show a, Show b) => (a -> b) -> Maybe a -> IO (Maybe a)
testSolution test solution = case solution of
  Nothing -> do putStrLn "No solution found"
                return Nothing
  Just s  -> do putStrLn $ "Solution: " ++ (show s)
                putStr "Test: "
                hFlush stdout 
                putStrLn $ show $ test s
                return $ Just s

handleResult :: (Primitive p, MonadSAT m, Decode m (EncodedAdt p) a)
             => EncodedAdt p -> EncodedAdt p -> m (Maybe (m a))
handleResult unknown result = do
  case E.flags result of
    Nothing -> do
      note "Error: missing flags in constraint system's result (maybe 'undefined' or 'bottom')"
      return Nothing

    Just flags ->
      case E.constantConstructorIndex result of
        Just 0 -> do 
          note "Known result: unsatisfiable"
          return Nothing

        Just 1 -> do 
          note "Known result: valid"
          return Nothing

        Nothing -> do
          formula <- and [ head flags , E.definedness result ]
          assert [ formula ]
          return $ Just $ decode unknown 

        _ -> do 
          note "Error: constraint system did not evaluate to a Boolean"
          return Nothing
