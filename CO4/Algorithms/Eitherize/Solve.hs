{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CO4.Algorithms.Eitherize.Solve
  (solve,solveAndTest)
where

-- import           Debug.Trace (traceShow)
import           System.IO (hFlush,stdout)
import qualified Satchmo.SAT.Mini as Backend 
import           Satchmo.Code (Decode,decode)
import           Satchmo.Boolean (assert)
import           CO4.EncodedAdt (EncodedAdt,unknown,flags)
import           CO4.AdtIndex (Indexed (index))
import           CO4.Algorithms.Eitherize.UnsizedAdt (UnsizedAdt)

-- |Main entry point for solving an encoded constraint system
solve :: (Indexed a, Decode Backend.SAT EncodedAdt (UnsizedAdt a)) 
      => a -> (EncodedAdt -> Backend.SAT EncodedAdt) -> IO (Maybe (UnsizedAdt a))
solve (undef :: a) constraint = 
  Backend.solve $ do u      <- unknown $ index 0 undef
                     result <- {-traceShow u $-} constraint u
                     assert [ head $ flags result ]
                     return ( ( decode u ) :: Backend.SAT (UnsizedAdt a))

solveAndTest :: ( Indexed a, Decode Backend.SAT EncodedAdt (UnsizedAdt a)
                , Show (UnsizedAdt a), Show b) 
      => a 
      -> (EncodedAdt -> Backend.SAT EncodedAdt) 
      -> (UnsizedAdt a -> b)
      -> IO ()
solveAndTest (undef :: a) constraint test = do
  solution <- Backend.solve $ do 
                   u      <- unknown $ index 0 undef
                   result <- {-traceShow u $-} constraint u
                   assert [ head $ flags result ]
                   return ( ( decode u ) :: Backend.SAT (UnsizedAdt a))
  case solution of
    Nothing ->    putStrLn "No solution found"
    Just s  -> do putStrLn $ "Solution: " ++ (show s)
                  putStr "Test: "
                  hFlush stdout 
                  putStrLn $ show $ test s
