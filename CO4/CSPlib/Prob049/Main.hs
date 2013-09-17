{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding ( sum )

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.PreludeNat
import           CO4.Util (toBinary,fromBinary)

import System.Environment (getArgs)
import System.IO

$( compileFile [ ImportPrelude
                        -- , DumpAll "/tmp/WCB"
                        , Cache
                        , Profile
                        ] 
  "CO4/CSPlib/Prob049/Constraint.hs" )


main = do
    [ arg1 ] <- getArgs
    run ( read arg1 :: Integer )

run n = do
    let bits = 3 * (  truncate $ logBase 2 
                   $ fromIntegral n )
    out <- solveAndTestP 
       ( nat bits 0 , nat bits 1
       , map (nat bits) [ 1 .. n ] )
       ( kList n uBool )
       encConstraint
       constraint
    print out
