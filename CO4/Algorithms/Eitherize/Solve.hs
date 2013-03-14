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
import           CO4.EncodedAdt (EncodedAdt,unknown,flags)
import           CO4.AdtIndex (Indexed (index))
import           CO4.Algorithms.Eitherize.UnsizedAdt (UnsizedAdt)

-- | Equals 'solveAndTest'. Uses 'Boolean's for encoding.
solveAndTestBoolean 
  :: ( Indexed a, Decode Backend.SAT (EncodedAdt Boolean) (UnsizedAdt a)
     , Show (UnsizedAdt a), Show b) 
  => Bool                                           
  -> a                                              
  -> ((EncodedAdt Boolean) -> Backend.SAT (EncodedAdt Boolean)) 
  -> (UnsizedAdt a -> b)                            
  -> IO ()
solveAndTestBoolean = solveAndTest

-- | Equals 'solveAndTest'. Uses 'Formula's for encoding.
solveAndTestFormula 
  :: ( Indexed a, Decode Backend.SAT (EncodedAdt Formula) (UnsizedAdt a)
     , Show (UnsizedAdt a), Show b) 
  => Bool                                           
  -> a                                              
  -> ((EncodedAdt Formula) -> Backend.SAT (EncodedAdt Formula)) 
  -> (UnsizedAdt a -> b)                            
  -> IO ()
solveAndTestFormula = solveAndTest

-- |Solves an encoded constraint system
solve :: ( Indexed a, Primitive p, Show p
         , Decode Backend.SAT (EncodedAdt p) (UnsizedAdt a)) 
      => Bool                                           -- ^Be verbosely
      -> a                                              -- ^@undefined :: a@
      -> ((EncodedAdt p) -> Backend.SAT (EncodedAdt p)) -- ^Encoded constraint system
      -> IO (Maybe (UnsizedAdt a))
solve verbose (undef :: a) constraint = 
  Backend.solve verbose $ do 
    u <- unknown $ index 0 undef
    {-
    when verbose $ do Backend.note "Unknown: "
                      Backend.note $ show u
                      -}
    result <- constraint u

    let assertion = head $ flags result
    when verbose $ Backend.note $ "Assertion: " ++ (show assertion)
    assert [ assertion ]
    return ( ( decode u ) :: Backend.SAT (UnsizedAdt a))

-- |Solves an encoded constraint system and tests the found solution
-- against the original constraint system
solveAndTest :: ( Indexed a, Primitive p, Show p
                , Decode Backend.SAT (EncodedAdt p) (UnsizedAdt a)
                , Show (UnsizedAdt a), Show b) 
      => Bool                                           -- ^Be verbosely
      -> a                                              -- ^@undefined :: a@ 
      -> ((EncodedAdt p) -> Backend.SAT (EncodedAdt p)) -- ^Encoded constraint system
      -> (UnsizedAdt a -> b)                            -- ^Original constraint system
      -> IO ()
solveAndTest verbose (undef :: a) constraint test = do
  solution <- solve verbose undef constraint
  case solution of
    Nothing ->    putStrLn "No solution found"
    Just s  -> do putStrLn $ "Solution: " ++ (show s)
                  putStr "Test: "
                  hFlush stdout 
                  putStrLn $ show $ test s
