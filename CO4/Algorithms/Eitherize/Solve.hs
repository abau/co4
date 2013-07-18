{-# LANGUAGE FlexibleContexts #-}
module CO4.Algorithms.Eitherize.Solve
 ( ConstraintSystem, ParamConstraintSystem
 , solveAndTestP, solveP
 , solveAndTest, solve
 )
where

import           Prelude hiding (and)
import           System.IO (hFlush,stderr,hPutStrLn,hPutStr)
import           Satchmo.Core.SAT.Minisat (note,solve')
import           Satchmo.Core.Decode (Decode,decode)
import           Satchmo.Core.Primitive (Primitive,assert,and)
import           CO4.Monad (CO4,SAT,runCO4)
import           CO4.EncodedAdt (EncodedAdt,flags,definedness,constantConstructorIndex)
import           CO4.Allocator (Allocator (..))
import           CO4.Encodeable (Encodeable(..))

type ConstraintSystem      = EncodedAdt -> CO4 EncodedAdt 
type ParamConstraintSystem = EncodedAdt -> ConstraintSystem 

-- |Solves a parametrized constraint system and tests the 
-- found solution against the original constraint system
solveAndTestP :: (Show a, Show b, Decode SAT EncodedAdt a, Encodeable k) 
              => k -> Allocator -> ParamConstraintSystem -> (k -> a -> b) 
              -> IO (Maybe a) 
solveAndTestP k allocator constraint test =
  solveP k allocator constraint >>= testSolution (test k)

-- |Solves a parametrized constraint system
solveP :: (Decode SAT EncodedAdt a, Encodeable k)
       => k -> Allocator -> ParamConstraintSystem -> IO (Maybe a)
solveP k allocator constraint = 
  solve' True $ do 
    (unknown,result) <- runCO4 $ do 
      unknown <- encode allocator
      param   <- encode k
      --note $ "Encoded unknown:\n" ++ show unknown
      result <- constraint param unknown
      return (unknown, result)
    handleResult unknown result

-- |Solves an constraint system and tests the found solution
-- against the original constraint system
solveAndTest :: (Show a, Show b, Decode SAT EncodedAdt a) 
             => Allocator -> ConstraintSystem -> (a -> b) -> IO (Maybe a)
solveAndTest allocator constraint test = 
  solve allocator constraint >>= testSolution test

-- |Solves a constraint system
solve :: (Decode SAT EncodedAdt a) => Allocator -> ConstraintSystem -> IO (Maybe a)
solve allocator constraint =
  solve' True $ do 
    (unknown,result) <- runCO4 $ do 
      unknown <- encode allocator
      --note $ "Encoded unknown:\n" ++ show unknown
      result <- constraint unknown
      return (unknown, result)
    handleResult unknown result

testSolution :: (Show a, Show b) => (a -> b) -> Maybe a -> IO (Maybe a)
testSolution test solution = case solution of
  Nothing -> do hPutStrLn stderr "No solution found"
                return Nothing
  Just s  -> do hPutStrLn stderr $ "Solution: " ++ (show s)
                hPutStr stderr "Test: "
                hFlush stderr
                hPutStrLn stderr $ show $ test s
                return $ Just s

handleResult :: (Decode SAT EncodedAdt a) => EncodedAdt -> EncodedAdt 
                                          -> SAT (Maybe (SAT a))
handleResult unknown result = do
  case flags result of
    Nothing -> do
      note "Error: missing flags in constraint system's result (maybe 'undefined' or 'bottom')"
      return Nothing

    Just flags ->
      case constantConstructorIndex result of
        Just 0 -> do 
          note "Known result: unsatisfiable"
          return Nothing

        Just 1 -> do 
          note "Known result: valid"
          return Nothing

        Nothing -> do
          formula <- and [ head flags , definedness result ]
          assert [ formula ]
          return $ Just $ decode unknown 

        _ -> do 
          note "Error: constraint system did not evaluate to a Boolean"
          return Nothing
