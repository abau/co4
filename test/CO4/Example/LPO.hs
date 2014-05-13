{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Example.LPO
where

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Example.LPOStandalone

$( compileFile [Cache,ImportPrelude] "test/CO4/Example/LPOStandalone.hs" )

aSym   = nat 2 0
sSym   = nat 2 1
nSym   = nat 2 2
trsAck = 
  let a = Node aSym
      s = Node sSym
      n = Node nSym []
      x = Var  (nat 1 0)
      y = Var  (nat 1 1)
  in
    [ (a [n,y]         , s [y])
    , (a [s [x], n]    , a [x, s [n]])
    , (a [s [x], s [y]], a [x, a [s [x], y]])
    ]

allocator = allocatorList [ knownTuple2 (fromKnown aSym) (uNat 2)
                          , knownTuple2 (fromKnown sSym) (uNat 2)
                          , knownTuple2 (fromKnown nSym) (uNat 2)
                          ]

result = solveAndTestP trsAck allocator encConstraint constraint
