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
    constraint p (x,y) = and [ (timesNat8 x y) == p
                             , gtNat8 x (nat8 1)
                             , gtNat8 y (nat8 1)
                             ]
   |] >>= runIO . configurable [ImportPrelude] . compile 
  )

allocator = uTuple2 uNat8 uNat8

result p = solveAndTestP (nat8 p) allocator encConstraint constraint
