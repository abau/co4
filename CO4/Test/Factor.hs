{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.PreludeNat
import           CO4.Util (toBinary,fromBinary)

import System.Environment (getArgs)
import System.IO

w = 24

$( [d| constraint p (x,y) = 
         gtNat x (nat 24 1) && gtNat y (nat 24 1) && eqNat p ( timesNat x y)
   |] >>= runIO . configurable [ ImportPrelude
                         , Profile, Cache
                        ] 
         . compile )

-- uNat w = kList w uBool

result x = do
    -- let xs = toBin x ; w = length xs
    solution <- solveAndTestP 
       (nat w x)
       ( known 0 1 [ uNat w, uNat w] )
       encConstraint constraint
    print solution

mainz = do
    [ s ] <- getArgs
    result $ read s

