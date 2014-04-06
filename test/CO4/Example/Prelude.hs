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
    data Color = Red 
               | Green 
               | Blue
               | Mix [Color]

    constraint c = case c of
      Blue  -> True
      Mix m -> case m of []   -> False
                         c:cs -> case c of 
                          Green -> True
                          _     -> False
      _     -> False

   |] >>= compile [ImportPrelude] 
  )

uColor 0 = constructors [ Just [], Just [], Just [], Nothing ]
uColor i = constructors [ Just [], Just [], Just [], Just [ uList 3 (uColor (i-1)) ] ]

allocator = uColor 1

result = solveAndTest allocator encConstraint constraint
