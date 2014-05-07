{-# LANGUAGE FlexibleContexts #-}
module CO4.Solve
 ( ConstraintSystem, ParamConstraintSystem
 , solveAndTestP, solveP
 , solveAndTest, solve
 )
where

import           Prelude hiding (and)
import           System.IO (hFlush,stderr,hPutStrLn,hPutStr)
import           Unsafe.Coerce (unsafeCoerce)
import           Satchmo.Core.MonadSAT (traced)
import           Satchmo.Core.SAT.Minisat (note,solve')
import           Satchmo.Core.Decode (Decode,decode)
import           Satchmo.Core.Primitive (assert,and)
import           CO4.Monad (CO4,SAT,runCO4)
import           CO4.EncodedAdt (EncodedAdt,flags,definedness,constantConstructorIndex)
import           CO4.Allocator (Allocator (..))
import           CO4.Encodeable (Encodeable(..))

type ConstraintSystem      = EncodedAdt -> CO4 EncodedAdt 
type ParamConstraintSystem = EncodedAdt -> ConstraintSystem 

-- |Solves a parametrized constraint system and tests the 
-- found solution against the original constraint system
solveAndTestP :: (Decode SAT EncodedAdt a, Encodeable k) 
              => k -> Allocator -> ParamConstraintSystem -> (k -> a -> b) 
              -> IO (Maybe a) 
solveAndTestP k allocator constraint test = do
  solution <- solveP k allocator constraint 
  testSolution (test k) solution
  return solution

-- |Solves a parametrized constraint system
solveP :: (Decode SAT EncodedAdt a, Encodeable k)
       => k -> Allocator -> ParamConstraintSystem -> IO (Maybe a)
solveP k allocator constraint = 
  solve' True $ do 
    (unknown,result) <- runCO4 $ do 
      unknown <- traced "Allocator:" $ encode allocator
      param   <- encode k
      --note $ "Encoded parameter:\n" ++ show param
      --note $ "Encoded unknown:\n" ++ show unknown
      result <- constraint param unknown
      return (unknown, result)
    handleResult unknown result

-- |Solves an constraint system and tests the found solution
-- against the original constraint system
solveAndTest :: (Decode SAT EncodedAdt a) 
             => Allocator -> ConstraintSystem -> (a -> b) -> IO (Maybe a)
solveAndTest allocator constraint test = do
  solution <- solve allocator constraint
  testSolution test solution
  return solution

-- |Solves a constraint system
solve :: (Decode SAT EncodedAdt a) => Allocator -> ConstraintSystem -> IO (Maybe a)
solve allocator constraint =
  solve' True $ do 
    (unknown,result) <- runCO4 $ do 
      unknown <- traced "Allocator:" $ encode allocator
      --note $ "Encoded unknown:\n" ++ show unknown
      result <- constraint unknown
      return (unknown, result)
    handleResult unknown result

testSolution :: (a -> b) -> Maybe a -> IO ()
testSolution test solution = case solution of
  Nothing ->    return ()
  Just s  -> do hPutStr stderr "Test: "
                hFlush stderr
                case unsafeCoerce (test s) of
                  False -> do hPutStrLn stderr $ show False
                              error "Solve.testSolution: abort due to previous test failure"
                  True  -> hPutStrLn stderr $ show True

handleResult :: (Decode SAT EncodedAdt a) => EncodedAdt -> EncodedAdt 
                                          -> SAT (Maybe (SAT a))
handleResult unknown result = do
  case flags result of
    Nothing -> do
      note "Error: missing flags in constraint system's result (maybe 'undefined' or 'empty')"
      return Nothing

    Just [flag] ->
      case constantConstructorIndex 2 result of
        Just 0 -> do 
          note "Known result: unsatisfiable"
          return Nothing

        Just 1 -> do 
          note "Known result: valid"
          return Nothing

        Nothing -> traced "Toplevel:" $ do
          formula <- and [ flag , definedness result ]
          assert [ formula ]
          return $ Just $ decode unknown 

    _ -> do 
      note "Error: constraint system did not evaluate to a Boolean"
      return Nothing
