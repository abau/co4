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
                        -- , Cache
                        -- , Profile
                        ] 
  "CO4/Test/Costas/Constraint.hs" )


main = do
    [ arg1 ] <- getArgs
    run ( read arg1 :: Int )

run length = do
    let bits = succ
             $ truncate $ logBase 2 
             $ fromIntegral length
    out <- solveAndTestP 
       True
       ( kList length $ known 0 1 [ uNat bits , uNat bits ] )
       encConstraint
       constraint
    print out
