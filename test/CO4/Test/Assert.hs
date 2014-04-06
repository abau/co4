{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- module CO4.Test.WCB where
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

$( runIO $ configurable [ ImportPrelude
                        -- , DumpAll "/tmp/WCB"
                        , Cache
                        , Profile
                        ] 
  $ compileFile "CO4/Test/Assert.standalone.hs" )


kList 0 a = known 0 2 []
kList i a = known 1 2 [ a , kList (i-1) a]

main = do
    out <- solveAndTestP 
       ( nat8 241 )
       ( kList 10 uNat8 )
       encConstraint
       constraint
    print out

