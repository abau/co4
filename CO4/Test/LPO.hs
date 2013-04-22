{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module CO4.Test.LPO where

import qualified Prelude ; import Prelude (($), (-), (*))

import qualified Data.Maybe as M

import           Language.Haskell.TH (runIO)

import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4

$( runIO $ configurable [Verbose, DumpAll "/tmp/lpo"] $ compileFile "CO4/Test/LPO.standalone.hs" )


uSymbol = constructors [ M.Just [], M.Just [], M.Just [], M.Just [] ]

result = solveAndTestBoolean (uList 4 uSymbol) encMain main
