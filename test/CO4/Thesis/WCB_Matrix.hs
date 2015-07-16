{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CO4.Thesis.WCB_Matrix
where

import Prelude hiding (sequence)

import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude.Nat
import           CO4.Thesis.WCB_MatrixStandalone

import           CO4.Prelude

$( compileFile [ ImportPrelude, InstantiationDepth 20 ] 
  "test/CO4/Thesis/WCB_MatrixStandalone.hs" )

uBase = unions [knownA, knownC, knownG, knownU]
uParen = unions [knownOpen, knownClose, knownBlank]
uEnergy = unions [knownMinusInfinity, knownFinite $ uNat 8]

uMatrix xs ys f =
    let row xs g = case xs of
            []    -> knownNill
            x:xs' -> knownConss (g x) (row xs' g)
    in 
      row xs $ \ x -> row ys $ \ y -> f x y

uTriagGap delta n = uMatrix [1 .. n] [1 .. n] $ \ i j ->
     if i + delta <= j 
     then knownFinite (uNat 8)
     else knownMinusInfinity

kList' 0 _ = knownNill
kList' i x = knownConss x (kList' (i-1) x)

toList :: [a] -> List a
toList = foldr Conss Nill

ex1 = toList [ Open , Open , Open , Open , Open
             , Blank, Blank, Blank, Open , Open
             , Open , Open , Open , Blank, Blank
             , Blank, Blank, Blank, Blank, Close
             , Close, Close, Close, Close, Blank
             , Close, Close, Close, Close, Close ]

ex0 = toList [ Open, Open , Blank, Close, Open , Close , Close, Blank ]

{-
result :: List Paren -> IO (Maybe (Pair (List Base) Energy))
result sec = do
  let n = foldr' (const succ) 0 sec
  out <- solveAndTestP sec 
                       (knownPair (kList' n uBase) (uTriagGap 1 (n+1)) )
                       encConstraint constraint
  return $ case out of
    Nothing -> Nothing
    Just (Pair p m) -> Just (Pair p (upright m))
    -}

resultInverse :: List Base -> IO (Maybe (Pair (List Paren) Energy))
resultInverse prim = do
  let n = foldr' (const succ) 0 prim
  out <- solveAndTestP prim
                       (knownPair (kList' n uParen) (uTriagGap 1 (n+1)) )
                       encConstraint constraint
  return $ case out of
    Nothing -> Nothing
    Just (Pair s m) -> Just (Pair s (upright m))
