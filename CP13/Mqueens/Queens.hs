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

import qualified Data.Map as M
import Data.List ( sort )

$( compileFile [ ImportPrelude
               --, Profile
               , Cache
               -- ,DumpAll "/tmp/WCB"
               ] 
   "CP13/Mqueens/Queens.standalone.hs")

run n k = do
    out <-  solveAndTestP        
       ( nat 8 n
       , do x <- [1..n] ; y <- [1..n] ; 
            return (nat 8 x, nat 8 y) )
       (kList k $ known 0 1 [ uNat 8, uNat 8 ])
       encConstraint constraint
    case out of
        Just queens -> do
            let m = M.fromListWith (error "huh") queens
                ys = do x <- map (nat 8) [1..n] 
                        let y = M.findWithDefault (nat 8 0) x m
                        return $ value y
            print $ ys


main = do
    [n,k] <- getArgs
    run (read n) (read k)


