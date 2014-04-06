{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Language.Haskell.TH (runIO)
import           Language.Haskell.TH.Syntax (addDependentFile)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Util (toBinary,fromBinary)

import System.Environment (getArgs)
import System.IO

$( compileFile [ ImportPrelude
               --, Profile
               , Cache
               -- ,DumpAll "/tmp/WCB"
               ] 
   "CO4/Test/Queens.standalone.hs")

$(addDependentFile "CO4/Test/Queens.standalone.hs" >> return [])

result n = do
    hSetBuffering stdout LineBuffering
    solution <- solveAndTestP 
       (nat 8 n)
       (kList n (uNat 8))
       encConstraint constraint
    case solution of
        Nothing -> putStrLn "no placement"
        Just xs -> print xs

main = do
    [s] <- getArgs
    result $ read s

