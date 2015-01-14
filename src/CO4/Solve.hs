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
import           Satchmo.Core.Primitive (assert)
import           CO4.Monad (CO4,SAT,runCO4)
import           CO4.EncodedAdt (EncodedAdt,flags,constantConstructorIndex,arguments)
import           CO4.Allocator (TAllocator)
import           CO4.Encodeable (Encodeable(..))
import           CO4.Algorithms.UndefinedValues.Data (completelyDefined)

type ConstraintSystem      = EncodedAdt -> CO4 EncodedAdt 
type ParamConstraintSystem = EncodedAdt -> ConstraintSystem 

-- |Solves a parametrized constraint system and tests the 
-- found solution against the original constraint system
solveAndTestP :: (Decode SAT EncodedAdt a, Encodeable k) 
              => k -> TAllocator a -> ParamConstraintSystem -> (k -> a -> b) 
              -> IO (Maybe a) 
solveAndTestP k allocator constraint test = do
  solution <- solveP k allocator constraint 
  testSolution (test k) solution
  return solution

-- |Solves a parametrized constraint system
solveP :: (Decode SAT EncodedAdt a, Encodeable k)
       => k -> TAllocator a -> ParamConstraintSystem -> IO (Maybe a)
solveP k allocator constraint = 
  solve' True $ do 
    (unknown,result) <- runCO4 $ do 
      unknown    <- traced "Allocator:" $ encode allocator
      optUnknown <- completelyDefined unknown
      param      <- encode k >>= completelyDefined
      --note $ "Encoded parameter:\n" ++ show param
      --note $ "Encoded unknown:\n" ++ show unknown
      result     <- constraint param optUnknown
      return (unknown, result)
    handleResult unknown result

-- |Solves an constraint system and tests the found solution
-- against the original constraint system
solveAndTest :: (Decode SAT EncodedAdt a) 
             => TAllocator a -> ConstraintSystem -> (a -> b) -> IO (Maybe a)
solveAndTest allocator constraint test = do
  solution <- solve allocator constraint
  testSolution test solution
  return solution

-- |Solves a constraint system
solve :: (Decode SAT EncodedAdt a) => TAllocator a -> ConstraintSystem -> IO (Maybe a)
solve allocator constraint =
  solve' True $ do 
    (unknown,result) <- runCO4 $ do 
      unknown    <- traced "Allocator:" $ encode allocator
      optUnknown <- completelyDefined unknown
      --note $ "Encoded unknown:\n" ++ show unknown
      result <- constraint optUnknown
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
handleResult unknown optResult = do
  case flags optResult of
    Nothing -> abortWith "Error: missing flags in constraint's result (optional Boolean)"

    Just [optFlag] -> 
      let optIndex = constantConstructorIndex 2 optResult
      in
        case optIndex of
          Just 0 -> abortWith "Error: constraint evaluated to 'undefined'"

          _ -> case arguments optResult of
            Nothing -> abortWith "Error: constraint did not evaluate to a Boolean (missing argument)"

            Just [boolResult] ->
              case flags boolResult of
                Nothing -> abortWith "Error: missing flags in constraint's result (Boolean)"

                Just [boolFlag] ->
                  case constantConstructorIndex 2 boolResult of
                    Just 0 -> abortWith "Known result: unsatisfiable"
                    Just 1 | optIndex == Just 1 -> abortWith "Known result: valid"

                    _ -> traced "Toplevel:" $ do
                      assert [ optFlag ]
                      assert [ boolFlag ]
                      return $ Just $ decode unknown 

                _ -> abortWith "Error: constraint system did not evaluate to a Boolean (too many arguments in Bool)"

            _ -> abortWith "Error: constraint did not evaluate to a Boolean (too many arguments in optional Bool)"
  where
    abortWith msg = note msg >> return Nothing
