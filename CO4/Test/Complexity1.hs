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

import qualified Data.Map as M
import Control.Monad ( void, forM )

import System.Environment (getArgs)
import System.IO

$( [d| 
    data T = A | B Bool T | C T Bool

    constraint k xs = allgood k (build xs)

    build xs = case xs of
        [] -> A
        x : xs' -> 
            let t' = build xs' 
            in  case x of
                   False -> B x t'
                   True  -> C t' x

    allgood k t = case t of
        A -> k
        B x t' -> False
        C t' x -> allgood k t'

   |] >>= compile [ ImportPrelude, Profile, Cache ] )

main = run 4

run depth = solveAndTestP True (kList depth uBool)
        encConstraint constraint >>= print

    
