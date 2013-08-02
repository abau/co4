{-# LANGUAGE LambdaCase #-}
module Main where

import           System.Exit (exitSuccess,exitFailure)
import           System.IO (hFlush,stdout)
import qualified CO4.Example.Binary
import qualified CO4.Example.Nat
import qualified CO4.Example.Prelude
import qualified CO4.Example.Simple

main :: IO ()
main = do
  simpleTest "CO4.Example.Binary"  $ CO4.Example.Binary.result 143
  simpleTest "CO4.Example.Nat"     $ CO4.Example.Nat.result 143
  simpleTest "CO4.Example.Prelude" $ CO4.Example.Prelude.result
  simpleTest "CO4.Example.Simple"  $ CO4.Example.Simple.result
  exitSuccess

simpleTest :: String -> IO (Maybe a) -> IO ()
simpleTest name action = do
  putStrLn $ concat [ "## ", name, " ", replicate (70 - length name) '#' ]
  hFlush stdout
  action >>= \case
    Nothing -> do putStrLn $ name ++ " does not return a result. Aborting test ..."
                  exitFailure
    Just _  -> return ()
