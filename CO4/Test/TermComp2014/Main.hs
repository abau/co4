{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

import           Prelude hiding (lex,lookup,length,iterate)
import           Control.Exception (assert)
import           System.Environment (getArgs)
import           System.Exit (exitSuccess, exitFailure)
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Test.TermComp2014.Util 
  (SymbolMap,parseTrs,assignments,dpProblem,dpToTrs,removeStrongDecreasingRules,hasMarkedRule)
import           CO4.Test.TermComp2014.PPrint
import           CO4.Test.TermComp2014.Allocators (allocator)
import           CO4.Test.TermComp2014.Standalone

$( compileFile [Cache,ImportPrelude] "CO4/Test/TermComp2014/Standalone.hs" )

main :: IO ()
main = do
  [bitWidth, numPrecedences, filePath] <- getArgs
  resultFile (read bitWidth) (read numPrecedences) filePath

resultFile :: Int -> Int -> FilePath -> IO ()
resultFile bitWidth numPrecedences filePath = do
  (trs, symbolMap) <- parseTrs filePath

  putStrLn $ "Parsed:\n" ++ pprintUnlabeledTrs symbolMap trs
  putStrLn $ "DP-TRS:\n" ++ pprintDPTrs (const "") symbolMap (dpProblem trs)

  iterate symbolMap 1 bitWidth numPrecedences (dpProblem trs) >>= \case
    False -> putStrLn "don't know" >> exitFailure
    True  -> putStrLn "terminates" >> exitSuccess

iterate :: SymbolMap -> Int -> Int -> Int -> DPTrs () -> IO Bool
iterate symbolMap i bitWidth numPrecedences dp = 
  let sigmas    = assignments bitWidth $ dpToTrs dp
      parameter = (dp, sigmas)
  in do
    putStrLn $ "\n## " ++ show i ++ "st/th iteration ##########################\n"
    putStrLn $ "TRS:\n" ++ pprintDPTrs (const "") symbolMap dp

    case hasMarkedRule dp of
      False -> return True
      _     -> solveAndTestP parameter (allocator bitWidth numPrecedences dp) encConstraint constraint
       >>= \case
             Nothing -> return False
             Just (model,precedences) -> assert (not $ null delete) $ 
               do putStrLn $ "Model:\n" ++
                           ( pprintModel pprintMarkedSymbol symbolMap model )

                  putStrLn $ "Labeled Trs:\n" ++ ( pprintDPTrs pprintLabel symbolMap labeledTrs )

                  putStrLn $ "Precedences:\n" ++
                           ( unlines $ map 
                              (pprintPrecedence pprintMarkedSymbol pprintLabel symbolMap) precedences 
                           )

                  putStrLn $ "\nDeleted:\n" ++
                           ( unlines $ map (pprintDPRule (const "") symbolMap) delete )

                  iterate symbolMap (i+1) bitWidth numPrecedences dp'
               where
                 (dp', delete) = removeStrongDecreasingRules dp labeledTrs precedences

                 labeledTrs = makeLabeledTrs model dp sigmas
