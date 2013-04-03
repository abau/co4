{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CO4.Algorithms.Eitherize.Solve
   (solveAndTestBoolean, solveAndTestFormula, solve, solveAndTest)
where

import           Control.Monad (when)
import           System.IO (hFlush,stdout)
import qualified Satchmo.Core.SAT.Minisat as Backend 
import           Satchmo.Core.Decode (Decode,decode)
import           Satchmo.Core.Primitive (Primitive,assert)
import           Satchmo.Core.Boolean (Boolean)
import           Satchmo.Core.Formula (Formula)
import           CO4.EncodedAdt (EncodedAdt,encode,flags,isUnknown)
import           CO4.Allocator (Allocator)

-- | Equals 'solveAndTest'. Uses 'Boolean's for encoding.
solveAndTestBoolean 
  :: ( Decode Backend.SAT (EncodedAdt Boolean) a, Show a, Show b) 
  => Bool                                           
  -> Allocator                                      
  -> ((EncodedAdt Boolean) -> Backend.SAT (EncodedAdt Boolean)) 
  -> (a -> b)                            
  -> IO ()
solveAndTestBoolean = solveAndTest

-- | Equals 'solveAndTest'. Uses 'Formula's for encoding.
solveAndTestFormula 
  :: ( Decode Backend.SAT (EncodedAdt Formula) a, Show a, Show b) 
  => Bool                                           
  -> Allocator                                      
  -> ((EncodedAdt Formula) -> Backend.SAT (EncodedAdt Formula)) 
  -> (a -> b)                            
  -> IO ()
solveAndTestFormula = solveAndTest

-- |Solves an encoded constraint system
solve :: ( Primitive p, Show p
         , Decode Backend.SAT (EncodedAdt p) a) 
      => Bool                                           -- ^Be verbosely
      -> Allocator                                      -- ^Allocator
      -> ((EncodedAdt p) -> Backend.SAT (EncodedAdt p)) -- ^Encoded constraint system
      -> IO (Maybe a)
solve verbose allocator constraint = 
  Backend.solve verbose $ do 
    u <- encode allocator
    {-
    when verbose $ do Backend.note "Unknown: "
                      Backend.note $ show u
                      -}
    result <- constraint u

    case isUnknown result of
      False -> Backend.note "Known result"
      True  -> do
        let assertion = head $ flags result
        when verbose $ Backend.note $ "Assertion: " ++ (show assertion)
        assert [ assertion ]
    return $ decode u 

-- |Solves an encoded constraint system and tests the found solution
-- against the original constraint system
solveAndTest :: ( Primitive p, Show p
                , Decode Backend.SAT (EncodedAdt p) a
                , Show a, Show b) 
      => Bool                                           -- ^Be verbosely
      -> Allocator                                      -- ^Allocator
      -> ((EncodedAdt p) -> Backend.SAT (EncodedAdt p)) -- ^Encoded constraint system
      -> (a -> b)                                       -- ^Original constraint system
      -> IO ()
solveAndTest verbose allocator constraint test = do
  solution <- solve verbose allocator constraint
  case solution of
    Nothing ->    putStrLn "No solution found"
    Just s  -> do putStrLn $ "Solution: " ++ (show s)
                  putStr "Test: "
                  hFlush stdout 
                  putStrLn $ show $ test s
