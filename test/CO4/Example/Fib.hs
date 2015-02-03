{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Example.Fib
where

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude hiding (nat,uNat,Nat)

$( [d| 
    data Nat = Z | S Nat deriving Show

    constraint p n = eq p (fib n)

    fib x = 
      case x of
        Z    -> Z
        S x' -> case x' of
          Z     -> S Z
          S x'' -> let f1 = fib x'
                       f2 = fib x''
                   in
                     add f1 f2

    eq x y = case x of
      Z -> case y of Z -> True
                     _ -> False
      S x' -> case y of Z    -> False
                        S y' -> eq x' y'

    add x y = case x of
      Z    -> y
      S x' -> S (add x' y)

   |] >>= compile [ImportPrelude,Cache] 
  )

uNat 0 = knownZ
uNat i = union knownZ $ knownS $ uNat $ i - 1
allocator = uNat 4

nat 0 = Z
nat i = S $ nat $ i - 1

result p = solveAndTestP (nat p) allocator encConstraint constraint
