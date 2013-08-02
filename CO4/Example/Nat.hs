{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Example.Prelude
where

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude

$( [d| 
    constraint p (x,y) = and [ (timesNat x y) == p
                             , gtNat x (nat 8 1)
                             , gtNat y (nat 8 1)
                             ]
   |] >>= compile [ImportPrelude]
  )

allocator = uTuple2 (uNat 8) (uNat 8)

result p = solveAndTestP (nat 8 p) allocator encConstraint constraint
