{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CO4.Algorithms.Eitherize.Solve
   {-
   ( ConstraintSystem
   , solveAndTestBoolean, solveProfileAndTestBoolean
   , solveAndTestBooleanP, solveProfileAndTestBooleanP
   , solveAndTestFormula, solveProfileAndTestFormula
   , solveAndTestFormulaP, solveProfileAndTestFormulaP
   , solveAndTest, solveAndTestP , solve)
   -}
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
import           CO4.Cache (Cache,runCache)
import           CO4.Encodeable (Encodeable (..))
import           CO4.Profiling (SimpleProfiling, simpleProfiling)

import           CO4.Allocator.Overlapping ()
import           CO4.EncodedAdt.Overlapping (Overlapping)

type EncodedAdt = Overlapping

type ConstraintSystem m p      = (EncodedAdt p) -> m (EncodedAdt p)
type ParamConstraintSystem m p = (EncodedAdt p) -> ConstraintSystem m p

booleanCache :: (MonadIO m) => Cache (EncodedAdt Boolean) m a -> m a
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


          {-
type EncodedAdt = Overlapping

type ProfiledConstraintSystem p = (EncodedAdt p) 
                               -> SimpleProfiling (Cache (EncodedAdt p) Backend.SAT) 
                                                  (EncodedAdt p)

type ConstraintSystem p = (EncodedAdt p) 
                       -> Cache (EncodedAdt p) Backend.SAT (EncodedAdt p)

type ParamConstraintSystem p         = (EncodedAdt p) -> ConstraintSystem p
type ProfiledParamConstraintSystem p = (EncodedAdt p) -> ProfiledConstraintSystem p


-- | Equals 'solveAndTest'. Uses 'Boolean's for encoding.
solveAndTestBoolean 
  :: (Decode Backend.SAT (EncodedAdt Boolean) a, Show a, Show b) 
  => Allocator -> ConstraintSystem Boolean -> (a -> b) -> IO (Maybe a)
solveAndTestBoolean = solveAndTest

-- | Equals 'solveProfileAndTest'. Uses 'Boolean's for encoding.
solveProfileAndTestBoolean 
  :: (Decode Backend.SAT (EncodedAdt Boolean) a, Show a, Show b) 
  => Allocator -> ProfiledConstraintSystem Boolean -> (a -> b) -> IO (Maybe a)
solveProfileAndTestBoolean = solveProfileAndTest

-- | Equals 'solveAndTestP'. Uses 'Boolean's for encoding.
solveAndTestBooleanP
  :: ( Encodeable k EncodedAdt Boolean, Decode Backend.SAT (EncodedAdt Boolean) a
     , Show a, Show b) 
  => k -> Allocator -> ParamConstraintSystem Boolean -> (k -> a -> b) -> IO (Maybe a)
solveAndTestBooleanP = solveAndTestP

-- | Equals 'solveProfileAndTestP'. Uses 'Boolean's for encoding.
solveProfileAndTestBooleanP
  :: ( Encodeable k EncodedAdt Boolean, Decode Backend.SAT (EncodedAdt Boolean) a
     , Show a, Show b) 
  => k -> Allocator -> ProfiledParamConstraintSystem Boolean 
  -> (k -> a -> b) -> IO (Maybe a)
solveProfileAndTestBooleanP = solveProfileAndTestP

-- | Equals 'solveAndTest'. Uses 'Formula's for encoding.
solveAndTestFormula 
  :: (Decode Backend.SAT (EncodedAdt Formula) a, Show a, Show b) 
  => Allocator -> ConstraintSystem Formula -> (a -> b) -> IO (Maybe a)
solveAndTestFormula = solveAndTest

-- | Equals 'solveProfileAndTest'. Uses 'Formula's for encoding.
solveProfileAndTestFormula 
  :: (Decode Backend.SAT (EncodedAdt Formula) a, Show a, Show b) 
  => Allocator -> ProfiledConstraintSystem Formula -> (a -> b) -> IO (Maybe a)
solveProfileAndTestFormula = solveProfileAndTest

-- | Equals 'solveAndTestP'. Uses 'Formula's for encoding.
solveAndTestFormulaP 
  :: ( Encodeable k EncodedAdt Formula, Decode Backend.SAT (EncodedAdt Formula) a
     , Show a, Show b) 
  => k -> Allocator -> ParamConstraintSystem Formula -> (k -> a -> b) -> IO (Maybe a)
solveAndTestFormulaP = solveAndTestP

-- | Equals 'solveProfileAndTestP'. Uses 'Formula's for encoding.
solveProfileAndTestFormulaP 
  :: ( Encodeable k EncodedAdt Formula, Decode Backend.SAT (EncodedAdt Formula) a
     , Show a, Show b) 
  => k -> Allocator -> ProfiledParamConstraintSystem Formula 
  -> (k -> a -> b) -> IO (Maybe a)
solveProfileAndTestFormulaP = solveProfileAndTestP

-- |Solves an encoded constraint system and tests the found solution
-- against the original constraint system
solveAndTest :: ( Primitive p, Show p
                , Decode Backend.SAT (EncodedAdt p) a
                , Show a, Show b) 
      => Allocator                                      -- ^Allocator
      -> ConstraintSystem p                             -- ^Encoded constraint system
      -> (a -> b)                                       -- ^Original constraint system
      -> IO (Maybe a)
solveAndTest allocator constraint test = 
  solve allocator constraint >>= testSolution test

-- |Solves an encoded constraint system and tests the found solution
-- against the original constraint system
solveProfileAndTest :: ( Primitive p, Show p
                       , Decode Backend.SAT (EncodedAdt p) a
                       , Show a, Show b) 
      => Allocator                                      -- ^Allocator
      -> ProfiledConstraintSystem p                     -- ^Encoded constraint system
      -> (a -> b)                                       -- ^Original constraint system
      -> IO (Maybe a)
solveProfileAndTest allocator constraint test = 
  solveAndProfile allocator constraint >>= testSolution test

-- |Solves an encoded parametrized constraint system and tests the found solution
-- against the original constraint system
solveAndTestP :: ( Encodeable k EncodedAdt p, Primitive p, Show p
                 , Decode Backend.SAT (EncodedAdt p) a
                 , Show a, Show b) 
      => k                                              -- ^Known parameter
      -> Allocator                                      -- ^Allocator
      -> ParamConstraintSystem p                        -- ^Encoded constraint system
      -> (k -> a -> b)                                  -- ^Original constraint system
      -> IO (Maybe a)
solveAndTestP k allocator constraint test =
  solve allocator (constraint $ encodeConstant k) >>= testSolution (test k)

-- |Solves and profiles an encoded parametrized constraint system and tests the 
-- found solution against the original constraint system
solveProfileAndTestP :: ( Encodeable k EncodedAdt p, Primitive p, Show p
                        , Decode Backend.SAT (EncodedAdt p) a
                        , Show a, Show b) 
      => k                                              -- ^Known parameter
      -> Allocator                                      -- ^Allocator
      -> ProfiledParamConstraintSystem p                -- ^Encoded constraint system
      -> (k -> a -> b)                                  -- ^Original constraint system
      -> IO (Maybe a)
solveProfileAndTestP k allocator constraint test =
  solveAndProfile allocator (constraint $ encodeConstant k) >>= testSolution (test k)

testSolution :: (Show a, Show b) => (a -> b) -> Maybe a -> IO (Maybe a)
testSolution test solution = case solution of
  Nothing -> do putStrLn "No solution found"
                return Nothing
  Just s  -> do putStrLn $ "Solution: " ++ (show s)
                putStr "Test: "
                hFlush stdout 
                putStrLn $ show $ test s
                return $ Just s

-- |Solves an encoded constraint system
solve :: (Primitive p, Show p, Decode Backend.SAT (EncodedAdt p) a) 
      => Allocator                                      -- ^Allocator
      -> ConstraintSystem p                             -- ^Encoded constraint system
      -> IO (Maybe a)
solve allocator constraint = 
  Backend.solve' True $ do 
    unknown <- encode allocator
    --Backend.note $ "Encoded unknown:\n" ++ show unknown
    result <- runCache $ constraint unknown
    handleResult unknown result

-- |Solves and profiles an encoded constraint system
solveAndProfile :: (Primitive p, Show p, Decode Backend.SAT (EncodedAdt p) a) 
      => Allocator                                      -- ^Allocator
      -> ProfiledConstraintSystem p                     -- ^Encoded constraint system
      -> IO (Maybe a)
solveAndProfile allocator constraint = 
  Backend.solve' True $ do 
    unknown <- encode allocator
    --Backend.note $ "Encoded unknown:\n" ++ show unknown
    result <- runCache $ simpleProfiling $ constraint unknown
    handleResult unknown result
    -}
