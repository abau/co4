{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module CO4.Example.QueensSelfContained
where

import           Prelude (Int,Show (..),(-),(>>=),IO,($))
import qualified Data.Maybe as M
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4

$( [d|  data Bool   = False | True            deriving Show
        data Nat    = Z     | S Nat           deriving Show
        data List a = Nil   | Cons a (List a) deriving Show
        type Board  = List Nat

        constraint :: Board -> Bool
        constraint board = 
          let n = length board
          in
            and2 (all (\q -> less q n) board)
                 (allSafe board)

        allSafe :: Board -> Bool
        allSafe board = case board of 
          Nil       -> True
          Cons q qs -> and2 (safe q qs (S Z))
                            (allSafe qs)

        safe :: Nat -> Board -> Nat -> Bool
        safe q board delta = case board of
          Nil       -> True
          Cons x xs -> and2 (noAttack q x delta)
                            (safe q xs (S delta))

        noAttack :: Nat -> Nat -> Nat -> Bool
        noAttack x y delta = 
          and2 (noStraightAttack x y)
               (noDiagonalAttack x y delta)

        noStraightAttack :: Nat -> Nat -> Bool
        noStraightAttack x y = not (equal x y)

        noDiagonalAttack :: Nat -> Nat -> Nat -> Bool
        noDiagonalAttack x y delta = 
          and2 (not (equal x (add y delta)))
               (not (equal y (add x delta)))
         
        length :: List a -> Nat
        length xs = case xs of 
          Nil       -> Z
          Cons _ ys -> S (length ys)

        all :: (a -> Bool) -> List a -> Bool
        all f xs = case xs of
          Nil       -> True
          Cons y ys -> and2 (f y) (all f ys)

        add :: Nat -> Nat -> Nat
        add x y = case x of
          Z    -> y
          S x' -> S (add x' y)

        equal :: Nat -> Nat -> Bool
        equal x y = case x of
          Z    -> case y of Z    -> True
                            _    -> False
          S x' -> case y of S y' -> equal x' y'
                            _    -> False

        less :: Nat -> Nat -> Bool
        less x y = case x of
          Z    -> case y of Z    -> False
                            _    -> True
          S x' -> case y of S y' -> less x' y'
                            _    -> False

        not :: Bool -> Bool
        not x = case x of   
          False -> True
          True  -> False

        and2 :: Bool -> Bool -> Bool
        and2 x y = case x of
          False -> False
          True  -> y

   |] >>= compile [Cache]
  )

natAllocator 0 = knownZ
natAllocator i = union knownZ $ knownS $ natAllocator $ i-1

listAllocator 0 _ = knownNil
listAllocator i a = knownCons a (listAllocator (i-1) a)

result :: Int -> IO (M.Maybe Board)
result i = solveAndTest (listAllocator i (natAllocator i)) encConstraint constraint
