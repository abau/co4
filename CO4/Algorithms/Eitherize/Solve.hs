{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CO4.Algorithms.Eitherize.Solve
   ( ConstraintSystem
   , solveAndTestBoolean, solveAndTestBooleanP
   , solveAndTestFormula, solveAndTestFormulaP
   , solveAndTest, solveAndTestP, solve)
where

import           System.IO (hFlush,stdout)
import           Data.Maybe (fromJust)
import qualified Satchmo.Core.SAT.Minisat as Backend 
import           Satchmo.Core.Decode (Decode,decode)
import           Satchmo.Core.Primitive (Primitive,assert)
import           Satchmo.Core.Boolean (Boolean)
import           Satchmo.Core.Formula (Formula)
import           CO4.EncodedAdt (EncodedAdt)
import qualified CO4.EncodedAdt as E
import           CO4.Allocator (Allocator)
import           CO4.Cache (Cache,runCache)
import           CO4.Encodeable (Encodeable (..))
import           CO4.Profiling (SimpleProfiling, simpleProfiling)

type ConstraintSystem p = (EncodedAdt p) 
                       -> SimpleProfiling (Cache p Backend.SAT) (EncodedAdt p)

type ParamConstraintSystem p = (EncodedAdt p) -> ConstraintSystem p

-- | Equals 'solveAndTest'. Uses 'Boolean's for encoding.
solveAndTestBoolean 
  :: (Decode Backend.SAT (EncodedAdt Boolean) a, Show a, Show b) 
  => Allocator                                      
  -> ConstraintSystem Boolean
  -> (a -> b)                            
  -> IO ()
solveAndTestBoolean = solveAndTest

-- | Equals 'solveAndTestP'. Uses 'Boolean's for encoding.
solveAndTestBooleanP
  :: (Encodeable k, Decode Backend.SAT (EncodedAdt Boolean) a, Show a, Show b) 
  => k
  -> Allocator                                      
  -> ParamConstraintSystem Boolean
  -> (k -> a -> b)                            
  -> IO ()
solveAndTestBooleanP = solveAndTestP

-- | Equals 'solveAndTest'. Uses 'Formula's for encoding.
solveAndTestFormula 
  :: (Decode Backend.SAT (EncodedAdt Formula) a, Show a, Show b) 
  => Allocator                                      
  -> ConstraintSystem Formula
  -> (a -> b)                            
  -> IO ()
solveAndTestFormula = solveAndTest

-- | Equals 'solveAndTestP'. Uses 'Formula's for encoding.
solveAndTestFormulaP 
  :: (Encodeable k, Decode Backend.SAT (EncodedAdt Formula) a, Show a, Show b) 
  => k 
  -> Allocator                                      
  -> ParamConstraintSystem Formula
  -> (k -> a -> b)                            
  -> IO ()
solveAndTestFormulaP = solveAndTestP

-- |Solves an encoded constraint system and tests the found solution
-- against the original constraint system
solveAndTest :: ( Primitive p, Show p
                , Decode Backend.SAT (EncodedAdt p) a
                , Show a, Show b) 
      => Allocator                                      -- ^Allocator
      -> ConstraintSystem p                             -- ^Encoded constraint system
      -> (a -> b)                                       -- ^Original constraint system
      -> IO ()
solveAndTest allocator constraint test = do
  solution <- solve allocator constraint
  case solution of
    Nothing ->    putStrLn "No solution found"
    Just s  -> do putStrLn $ "Solution: " ++ (show s)
                  putStr "Test: "
                  hFlush stdout 
                  putStrLn $ show $ test s

-- |Solves an encoded parametrized constraint system and tests the found solution
-- against the original constraint system
solveAndTestP :: ( Encodeable k, Primitive p, Show p
                 , Decode Backend.SAT (EncodedAdt p) a
                 , Show a, Show b) 
      => k                                              -- ^Known parameter
      -> Allocator                                      -- ^Allocator
      -> ParamConstraintSystem p                        -- ^Encoded constraint system
      -> (k -> a -> b)                                  -- ^Original constraint system
      -> IO ()
solveAndTestP k allocator constraint test = do
  solution <- solve allocator $ constraint $ encodeConstant k
  case solution of
    Nothing ->    putStrLn "No solution found"
    Just s  -> do putStrLn $ "Solution: " ++ (show s)
                  putStr "Test: "
                  hFlush stdout 
                  putStrLn $ show $ test k s

-- |Solves an encoded constraint system
solve :: ( Primitive p, Show p
         , Decode Backend.SAT (EncodedAdt p) a) 
      => Allocator                                      -- ^Allocator
      -> ConstraintSystem p                             -- ^Encoded constraint system
      -> IO (Maybe a)
solve allocator constraint = 
  Backend.solve' True $ do 
    u <- encode allocator
    result <- runCache $ simpleProfiling (constraint u)

    if E.isBottom result
      then do 
        Backend.note "Error: (bottom) constraint system did not evaluate to a Boolean"
        return Nothing
      else
        case E.constantConstructorIndex result of
          Just 0 -> do 
            Backend.note "Known result: unsatisfiable"
            return Nothing

          Just 1 -> do 
            Backend.note "Known result: valid"
            return Nothing

          Nothing -> do
            let flag = head $ fromJust $ E.flags result
            Backend.note $ "Assertion: " ++ (show flag)
            assert [ flag ]
            return $ Just $ decode u 

          _ -> do 
            Backend.note "Error: constraint system did not evaluate to a Boolean"
            return Nothing
