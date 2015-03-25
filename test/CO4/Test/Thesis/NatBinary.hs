{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Test.Thesis.NatBinary
where

import           Prelude (Int,(-),undefined,(>>=),error,Show (..),putStrLn,(.),id)
import qualified Data.Maybe as M
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4

$( [d|  data Bool     = False | True deriving Show
        data List a   = Nil | Cons a (List a) deriving Show
        data Pair a b = Pair a b deriving Show

        constraint :: List Bool -> Pair (List Bool) (List Bool) -> Bool
        constraint p u = case u of
          Pair x y -> case add x y of
            Pair sum carry -> 

              and (eqNat sum p) (not carry)
            {-
              and (notTrivial x)
                  (and (notTrivial y)
                       (and (eqNat sum p) (not carry)))

        notTrivial :: List Bool -> Bool
        notTrivial x = foldr or False x
        -}

        add :: List Bool -> List Bool -> Pair (List Bool) Bool
        add x y = 
          let add' pair accu = case pair of
                Pair u v -> case accu of
                  Pair bits carry -> case fullAdder u v carry of
                    Pair sum carry' -> Pair (Cons sum bits) carry
          in
            foldr add' (Pair Nil False) (zip x y) 

        fullAdder :: Bool -> Bool -> Bool -> Pair Bool Bool
        fullAdder x y carry = case halfAdder x y of
          Pair sum1 carry1 -> case halfAdder sum1 carry of
            Pair sum2 carry2 -> Pair sum2 (or carry1 carry2)

        halfAdder :: Bool -> Bool -> Pair Bool Bool
        halfAdder x y = Pair (xor x y) (and x y)
          
        eqNat :: List Bool -> List Bool -> Bool
        eqNat x y = case x of
          Nil -> case y of Nil       -> True
                           Cons v vs -> False
          Cons u us -> case y of Nil       -> False
                                 Cons v vs -> and (eq u v) (eqNat us vs)

        foldr :: (a -> b -> b) -> b -> List a -> b
        foldr f accu xs = case xs of
          Nil       -> accu
          Cons y ys -> f y (foldr f accu ys)

        zip :: List a -> List b -> List (Pair a b)
        zip x y = case x of
          Nil -> Nil
          Cons u us -> case y of Nil       -> Nil
                                 Cons v vs -> Cons (Pair u v) (zip us vs)

        and :: Bool -> Bool -> Bool
        and x y = case x of
          False -> False
          True  -> y

        or :: Bool -> Bool -> Bool
        or x y = case x of
          False -> y
          True  -> True

        xor :: Bool -> Bool -> Bool
        xor x y = not (eq x y)

        eq :: Bool -> Bool -> Bool
        eq x y = case x of
          False -> not y
          True  -> y

        not :: Bool -> Bool
        not x = case x of False -> True
                          True  -> False

   |] >>= compile [Cache]
  )

uBool :: TAllocator Bool
uBool  = union knownFalse knownTrue

uNat :: Int -> TAllocator (List Bool)
uNat 0 = knownNil
uNat i = knownCons uBool (uNat (i-1))

allocator = knownPair (uNat 8) (uNat 8)

param :: List Bool
param = Cons False (Cons True  (Cons True  (Cons True (
        Cons True  (Cons False (Cons True  (Cons True Nil)))))))

result = solveAndTestP param allocator encConstraint constraint

{- 00111011 01000000 -}
