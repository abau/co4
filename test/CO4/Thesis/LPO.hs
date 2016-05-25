{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Thesis.LPO
where

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude.Nat
import           CO4.Thesis.LPOStandalone

$( compileFile [Cache,ImportPrelude] "test/CO4/Thesis/LPOStandalone.hs" )

(trsAck, allocatorAck) = 
  let a a1 a2 = Node (nat 2 0) (Conss a1 (Conss a2 Nill))
      s s1    = Node (nat 2 1) (Conss s1 Nill)
      n       = Node (nat 2 2) Nill
      x       = Var  (nat 1 0)
      y       = Var  (nat 1 1)
      
      syms    = Conss (nat 2 0)
              ( Conss (nat 2 1)
              ( Conss (nat 2 2) Nill))

      rules   = Conss (Pair (a n y)         (s y))
              ( Conss (Pair (a (s x) n)     (a x (s n)))
              ( Conss (Pair (a (s x) (s y)) (a x (a (s x) y))) Nill ))

      alloc   = knownConss (uNat 2)
              ( knownConss (uNat 2)
              ( knownConss (uNat 2) knownNill))
  in
    (TRS syms rules, alloc)

(trsF, allocatorF) = 
  let t  a b = Node (nat 3 0) (Conss a (Conss b Nill))
      p  a b = Node (nat 3 1) (Conss a (Conss b Nill))
      f  a   = Node (nat 3 2) (Conss a Nill)
      g  a b = Node (nat 3 3) (Conss a (Conss b Nill))
      a      = Node (nat 3 4) Nill
      x      = Var  (nat 2 0)
      y      = Var  (nat 2 1)
      z      = Var  (nat 2 2)

      syms    = Conss (nat 3 0)
              ( Conss (nat 3 1)
              ( Conss (nat 3 2)
              ( Conss (nat 3 3)
              ( Conss (nat 3 4) Nill))))

      rules  = Conss (Pair (t (t x y) z)     (t x (t y z)))
             ( Conss (Pair (t (p  x y) z)    (p (t x z) (t y z)))
             ( Conss (Pair (t x (p y (f z))) (t (g x z) (p y a))) Nill ))

      alloc  = knownConss (uNat 3)
             ( knownConss (uNat 3)
             ( knownConss (uNat 3)
             ( knownConss (uNat 3)
             ( knownConss (uNat 3) knownNill))))
  in
    (TRS syms rules, alloc)

(trsFSL, allocatorFSL) = 
  let t1 a b = Node (nat 3 0) (Conss a (Conss b Nill))
      t2 a b = Node (nat 3 1) (Conss a (Conss b Nill))
      p  a b = Node (nat 3 2) (Conss a (Conss b Nill))
      f  a   = Node (nat 3 3) (Conss a Nill)
      g  a b = Node (nat 3 4) (Conss a (Conss b Nill))
      a      = Node (nat 3 5) Nill
      x      = Var  (nat 2 0)
      y      = Var  (nat 2 1)
      z      = Var  (nat 2 2)

      syms    = Conss (nat 3 0)
              ( Conss (nat 3 1)
              ( Conss (nat 3 2)
              ( Conss (nat 3 3)
              ( Conss (nat 3 4)
              ( Conss (nat 3 5) Nill)))))

      rules  = Conss (Pair (t1 (t1 x y) z)     (t1 x (t1 y z)))
             ( Conss (Pair (t2 (t1 x y) z)     (t1 x (t2 y z)))
             ( Conss (Pair (t1 (t2 x y) z)     (t1 x (t1 y z)))
             ( Conss (Pair (t2 (t2 x y) z)     (t1 x (t2 y z)))
             ( Conss (Pair (t1 (p  x y) z)     (p (t1 x z) (t1 y z)))
             ( Conss (Pair (t2 (p  x y) z)     (p (t2 x z) (t2 y z)))
             ( Conss (Pair (t2 x (p y (f z)))  (t1 (g x z) (p y a))) Nill ))))))

      alloc  = knownConss (uNat 3)
             ( knownConss (uNat 3)
             ( knownConss (uNat 3)
             ( knownConss (uNat 3)
             ( knownConss (uNat 3)
             ( knownConss (uNat 3) knownNill)))))
  in
    (TRS syms rules, alloc)

(trsCF, allocatorCF) = 
  let f a = Node (nat 2 0) (Conss a Nill)
      s a = Node (nat 2 1) (Conss a Nill)
      g a = Node (nat 2 2) (Conss a Nill)
      x   = Var  (nat 2 0)

      syms    = Conss (nat 2 0)
              ( Conss (nat 2 1)
              ( Conss (nat 2 2) Nill))

      rules  = Conss (Pair (f (s x)) (s (g x)))
             ( Conss (Pair (g (s x)) (s (f x))) Nill)

      alloc  = knownConss (uNat 2)
             ( knownConss (uNat 2)
             ( knownConss (uNat 2) knownNill ))
  in
    (TRS syms rules, alloc)

result = solveAndTestP trsAck allocatorAck encConstraint constraint
