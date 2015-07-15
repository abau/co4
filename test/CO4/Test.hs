{-# LANGUAGE LambdaCase #-}
module Main where

import           System.Exit (exitSuccess,exitFailure)
import           System.IO (hFlush,stdout)
import           System.Environment (getArgs)
import qualified CO4.Example.Binary
import qualified CO4.Example.Nat
import qualified CO4.Example.Prelude
import qualified CO4.Example.Simple
--import qualified CO4.Example.LoopSrs
import qualified CO4.Example.LoopTrsToyama
import qualified CO4.Example.WCB_Matrix
import qualified CO4.Example.QueensSelfContained
import qualified CO4.Example.LPO
import qualified CO4.Example.LPOSL
import qualified CO4.Example.Hang
import qualified CO4.Example.Unary
import qualified CO4.Example.Fib

main :: IO ()
main = do 
  getArgs >>= \case
    ["binary"]      -> binary 143
    ["binary",n]    -> binary $ read n
    ["nat"]         -> nat 143
    ["nat",n]       -> nat $ read n
    ["prelude"]     -> prelude
    ["simple"]      -> simple
  --  ["loop-srs"]    -> loopSrs
    ["loop-trs"]    -> loopTrs
    ["wcb-matrix"]  -> wcbMatrix
    ["queens-self"] -> queensSelf 8
    ["lpo"]         -> lpo
    ["lpo-sl"]      -> lposl
    ["hang",s,n]    -> hang (read s) (read n)
    ["hang"]        -> hang 4 16
    ["unary",n]     -> unary $ read n
    ["unary"]       -> unary 15
    ["fib",n]       -> fib $ read n
    ["fib"]         -> fib 3
    _               -> all
  exitSuccess

  where
    binary  = simpleTest "CO4.Example.Binary"  . CO4.Example.Binary.result
    nat     = simpleTest "CO4.Example.Nat"     . CO4.Example.Nat.result
    prelude = simpleTest "CO4.Example.Prelude" $ CO4.Example.Prelude.result
    simple  = simpleTest "CO4.Example.Simple"  $ CO4.Example.Simple.result
    {-
    loopSrs = do simpleTest "CO4.Example.Loop: gebhardt-03"  
                  $ CO4.Example.LoopSrs.solve 16 16 "test/CO4/Example/LoopSrs/gebhardt-03.xml"
                 simpleTest "CO4.Example.Loop: gebhardt-08"  
                  $ CO4.Example.LoopSrs.solve 16 16 "test/CO4/Example/LoopSrs/gebhardt-08.xml"
                 simpleTest "CO4.Example.Loop: zantema_z042"  
                  $ CO4.Example.LoopSrs.solve 16 16 "test/CO4/Example/LoopSrs/zantema_z042.xml"
                 simpleTest "CO4.Example.Loop: zantema_loop1"  
                  $ CO4.Example.LoopSrs.solve 16 16 "test/CO4/Example/LoopSrs/zantema_loop1.xml"
                  -}

    loopTrs = simpleTest "CO4.Example.Loop: toyama" $ CO4.Example.LoopTrsToyama.result

    wcbMatrix = simpleTest "CO4.Example.WCB_Matrix: ex0" $ CO4.Example.WCB_Matrix.result
                                                         $ CO4.Example.WCB_Matrix.ex0

    queensSelf = simpleTest "CO4.Example.QueensSelfContained" . CO4.Example.QueensSelfContained.result

    lpo = simpleTest "CO4.Example.LPO" $ CO4.Example.LPO.result
    lposl = simpleTest "CO4.Example.LPOSL" $ CO4.Example.LPOSL.result

    hang s n = simpleTest "CO4.Example.Hang" $ CO4.Example.Hang.result s n

    unary = simpleTest "CO4.Example.Unary" . CO4.Example.Unary.result

    fib = simpleTest "CO4.Example.Fib" . CO4.Example.Fib.result

    all = binary 143 >> nat 143 >> prelude >> simple >> {-loopSrs >>-} loopTrs >> wcbMatrix
                     >> queensSelf 8 >> lpo >> lposl >> hang 4 16 >> unary 15 >> fib 3

simpleTest :: String -> IO (Maybe a) -> IO ()
simpleTest name action = do
  putStrLn $ concat [ "## ", name, " ", replicate (70 - length name) '#' ]
  hFlush stdout
  action >>= \case
    Nothing -> do putStrLn $ name ++ " does not return a result. Aborting test ..."
                  exitFailure
    Just _  -> return ()
