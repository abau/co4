{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CO4.Example.WCB_Matrix
  (result,ex0)
where

import Prelude hiding (sequence)

import           Control.Monad (void,forM)
import           Language.Haskell.TH (runIO)
import qualified Data.Map as M
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Util (toBinary,fromBinary)
import           System.Environment (getArgs)
import           System.IO
import           CO4.Example.WCB_MatrixStandalone

$( compileFile [ ImportPrelude
               -- , DumpAll "/tmp/WCB_Matrix"
               , InstantiationDepth 20
               ] 
  "test/CO4/Example/WCB_MatrixStandalone.hs" )

uBase = knownBase (kList 2 completeBool)

uEnergy = unions [knownMinusInfinity, knownFinite $ uNat 8]

uMatrix xs ys f =
    let row xs g = case xs of
            []    -> knownNil
            x:xs' -> knownCons (g x) (row xs' g)
    in  row xs $ \ x -> row ys $ \ y -> f x y

-- upper triag finite energy
uTriagGap delta n = uMatrix [1 .. n] [1 .. n] $ \ i j ->
     if i + delta <= j 
     then knownFinite (uNat 8)
     else knownMinusInfinity

uTriag2Gap delta n = uMatrix [1 .. n] [1 .. n] $ \ i j ->
     if i + delta <= j 
     then knownTuple2 (knownFinite $ uNat 8) uEnergy
     else knownTuple2 knownMinusInfinity knownMinusInfinity

inforna cs = map ( \ c -> case c of
    '(' -> Open ; '.' -> Blank ; ')' -> Close ) cs

ex1 = inforna "(((((...(((((......))))).)))))"

ex0 = [ Open, Open
      , Blank, Close, Open
      , Close , Close, Blank 
      ]

result :: [Paren] -> IO (Maybe [Base])
result sec = do
    let n = length sec
    out <- solveAndTestP 
       sec 
       ( knownTuple2 (kList n uBase) ( -- uTriag2Gap 1 (n+1) 
                                       uTriagGap 1 (n+1)
                                     ))
       encConstraint
       constraint
    return $ fmap fst out
