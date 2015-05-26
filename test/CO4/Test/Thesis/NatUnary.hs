{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module CO4.Test.Thesis.NatUnary
where

import           Prelude (undefined,(>>=),error,Show (..),putStrLn,($),(-))
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4

$( [d|  data Bool = False | True deriving Show
        data Nat = Z | S Nat deriving Show
        data Pair a b = Pair a b deriving Show

        constraint :: Nat -> Pair Nat Nat -> Bool
        constraint p u = case u of
          Pair a b -> let ab = plus a b
                      in
                        eq p ab

        plus :: Nat -> Nat -> Nat
        plus x y = case x of 
          Z -> y
          S x' -> S (plus x' y)

        eq :: Nat -> Nat -> Bool
        eq x y = case x of 
          Z -> case y of Z -> True
                         _ -> False
          S x' -> case y of Z    -> False
                            S y' -> eq x' y'
   |] >>= compile [Cache]
  )

uNat 0 = knownZ
uNat i = union knownZ $ knownS $ uNat $ i-1

kNat 0 = Z
kNat i = S $ kNat $ i-1

allocator = knownPair (uNat 1023) (uNat 1023)

result p = solveAndTestP (kNat p) allocator encConstraint constraint
