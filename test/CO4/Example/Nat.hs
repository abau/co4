{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Example.Nat
where

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude

$( [d| 
    constraint p (x,y) = and [ eqNat (timesNat x y) p
                             , gtNat x (nat 8 1)
                             , gtNat y (nat 8 1)
                             ]
   |] >>= compile [ImportPrelude]
  )

allocator = knownTuple2 (uNat 8) (uNat 8)

result p = solveAndTestP (nat 8 p) allocator encConstraint constraint
