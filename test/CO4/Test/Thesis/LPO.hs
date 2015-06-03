{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Test.Thesis.LPO
where

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude.Nat
import           CO4.Test.Thesis.LPOStandalone

$( compileFile [Cache,ImportPrelude] "test/CO4/Test/Thesis/LPOStandalone.hs" )

(trsAck, allocatorAck) = 
  let aSym    = nat 2 0
      sSym    = nat 2 1
      nSym    = nat 2 2
      a a1 a2 = Node aSym (Conss a1 (Conss a2 Nill))
      s s1    = Node sSym (Conss s1 Nill)
      n       = Node nSym Nill
      x       = Var (nat 1 0)
      y       = Var (nat 1 1)

      trs     = Conss (Pair (a n y)         (s y))
              ( Conss (Pair (a (s x) n)     (a x (s n)))
              ( Conss (Pair (a (s x) (s y)) (a x (a (s x) y))) Nill ))

      alloc   = knownConss (knownPair (fromKnown aSym) (uNat 2))
              ( knownConss (knownPair (fromKnown sSym) (uNat 2))
              ( knownConss (knownPair (fromKnown nSym) (uNat 2)) knownNill))
  in
    (trs, alloc)

(trsF, allocatorF) = 
  let tSym  = nat 3 0
      pSym  = nat 3 1
      fSym  = nat 3 2
      gSym  = nat 3 3
      aSym  = nat 3 4

      t  a b = Node tSym  (Conss a (Conss b Nill))
      p  a b = Node pSym  (Conss a (Conss b Nill))
      f  a   = Node fSym  (Conss a Nill)
      g  a b = Node gSym  (Conss a (Conss b Nill))
      a      = Node aSym  Nill
      x      = Var (nat 2 0)
      y      = Var (nat 2 1)
      z      = Var (nat 2 2)

      trs    = Conss (Pair (t (t x y) z)     (t x (t y z)))
             ( Conss (Pair (t (p  x y) z)     (p (t x z) (t y z)))
             ( Conss (Pair (t x (p y (f z)))  (t (g x z) (p y a))) Nill ))

      alloc  = knownConss (knownPair (fromKnown  tSym) (uNat 3))
             ( knownConss (knownPair (fromKnown  pSym) (uNat 3))
             ( knownConss (knownPair (fromKnown  fSym) (uNat 3))
             ( knownConss (knownPair (fromKnown  gSym) (uNat 3))
             ( knownConss (knownPair (fromKnown  aSym) (uNat 3)) knownNill))))
  in
    (trs, alloc)

(trsFSL, allocatorFSL) = 
  let t1Sym  = nat 3 0
      t2Sym  = nat 3 1
      pSym   = nat 3 2
      fSym   = nat 3 3
      gSym   = nat 3 4
      aSym   = nat 3 5

      t1 a b = Node t1Sym (Conss a (Conss b Nill))
      t2 a b = Node t2Sym (Conss a (Conss b Nill))
      p  a b = Node pSym  (Conss a (Conss b Nill))
      f  a   = Node fSym  (Conss a Nill)
      g  a b = Node gSym  (Conss a (Conss b Nill))
      a      = Node aSym  Nill
      x      = Var (nat 2 0)
      y      = Var (nat 2 1)
      z      = Var (nat 2 2)

      trs    = Conss (Pair (t1 (t1 x y) z)     (t1 x (t1 y z)))
             ( Conss (Pair (t2 (t1 x y) z)     (t1 x (t2 y z)))
             ( Conss (Pair (t1 (t2 x y) z)     (t1 x (t1 y z)))
             ( Conss (Pair (t2 (t2 x y) z)     (t1 x (t2 y z)))
             ( Conss (Pair (t1 (p  x y) z)     (p (t1 x z) (t1 y z)))
             ( Conss (Pair (t2 (p  x y) z)     (p (t2 x z) (t2 y z)))
             ( Conss (Pair (t2 x (p y (f z)))  (t1 (g x z) (p y a))) Nill ))))))

      alloc  = knownConss (knownPair (fromKnown t1Sym) (uNat 3))
             ( knownConss (knownPair (fromKnown t2Sym) (uNat 3))
             ( knownConss (knownPair (fromKnown  pSym) (uNat 3))
             ( knownConss (knownPair (fromKnown  fSym) (uNat 3))
             ( knownConss (knownPair (fromKnown  gSym) (uNat 3))
             ( knownConss (knownPair (fromKnown  aSym) (uNat 3)) knownNill)))))
  in
    (trs, alloc)

result = solveAndTestP trsFSL allocatorFSL encConstraint constraint
