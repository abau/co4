{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

import           Prelude hiding (lex,lookup,length,iterate)
import           Control.Exception (assert)
import           System.Environment (getArgs)
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Test.TermComp2014.Util 
  (parseTrs,assignments,dpProblem,dpToTrs,removeStrongDecreasingRules,hasMarkedRule)
import           CO4.Test.TermComp2014.PPrint
import           CO4.Test.TermComp2014.Allocators (allocator)
import           CO4.Test.TermComp2014.Standalone

$( compileFile [Cache,ImportPrelude] "CO4/Test/TermComp2014/Standalone.hs" )

main :: IO ()
main = do
  [n, filePath] <- getArgs
  resultFile (read n) filePath

resultFile :: Int -> FilePath -> IO ()
resultFile bitWidth filePath = do
  trs <- parseTrs filePath

  putStrLn $ "Parsed:\n" ++ pprintUnlabeledTrs trs
  putStrLn $ "DP-TRS:\n" ++ pprintDPTrs (const "") (dpProblem trs)

  iterate 1 bitWidth (dpProblem trs) >>= \case
    False -> putStrLn "don't know"
    True  -> putStrLn "terminates"

iterate :: Int -> Int -> DPTrs () -> IO Bool
iterate i bitWidth dp = 
  let sigmas    = assignments bitWidth $ dpToTrs dp
      parameter = (dp, sigmas)
  in do
    putStrLn $ "\n## " ++ show i ++ "st/th iteration ##########################\n"
    putStrLn $ "TRS:\n" ++ pprintDPTrs (const "") dp

    case hasMarkedRule dp of
      False -> return True
      _     -> solveAndTestP parameter (allocator bitWidth dp) encConstraint constraint
       >>= \case
             Nothing -> return False
             Just (model,precedence) -> assert (not $ null delete) $ 
               do putStrLn $ "Model:\n" ++
                           ( pprintModel pprintMarkedSymbol model )

                  putStrLn $ "Labeled Trs:\n" ++ ( pprintDPTrs pprintLabel labeledTrs )

                  putStrLn $ "Precedence:\n" ++
                           ( pprintPrecedence pprintMarkedSymbol pprintLabel precedence )

                  putStrLn $ "\nDeleted:\n" ++
                           ( unlines $ map (pprintDPRule $ const "") delete )

                  iterate (i+1) bitWidth dp'
               where
                 (dp', delete) = removeStrongDecreasingRules dp labeledTrs precedence

                 labeledTrs = makeLabeledTrs model dp sigmas
