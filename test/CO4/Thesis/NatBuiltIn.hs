{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Thesis.NatBuiltIn
where

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude

$( [d| 
    constraint p (x,y) = eqNat (plusNat x y) p
                         {-
                         and [ eqNat (plusNat x y) p
                             , gtNat x (nat 1)
                             , gtNat y (nat 1)
                             ]
                         -}
   |] >>= compile [ImportPrelude]
  )

allocator = knownTuple2 (uNat 10) (uNat 10)

result p = solveAndTestP (nat p) allocator encConstraint constraint
