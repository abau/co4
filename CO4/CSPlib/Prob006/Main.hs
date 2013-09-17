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
  "CO4/CSPlib/Prob006/Constraint.hs" )


main = do
    [ arg1, arg2 ] <- getArgs
    run ( read arg1 :: Int ) (read arg2 :: Integer)

run marks length = do
    let bits = succ
             $ truncate $ logBase 2 
             $ fromIntegral length
    out <- solveAndTestP 
       (nat bits length)
       ( kList (marks-1) $ uNat bits )
       encConstraint
       constraint
    print out
