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

aSym   = nat 2 0
sSym   = nat 2 1
nSym   = nat 2 2
trsAck = 
  let a a1 a2 = Node aSym (Conss a1 (Conss a2 Nill))
      s s1    = Node sSym (Conss s1 Nill)
      n       = Node nSym Nill
      x       = Var (nat 1 0)
      y       = Var (nat 1 1)
  in
      Conss (Pair (a n y)         (s y))
    ( Conss (Pair (a (s x) n)     (a x (s n)))
    ( Conss (Pair (a (s x) (s y)) (a x (a (s x) y))) Nill ))

allocator = knownConss (knownPair (fromKnown aSym) (uNat 2))
          ( knownConss (knownPair (fromKnown sSym) (uNat 2))
          ( knownConss (knownPair (fromKnown nSym) (uNat 2)) knownNill))

result = solveAndTestP trsAck allocator encConstraint constraint
