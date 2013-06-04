{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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

    main c = case c of
      Blue  -> True
      Mix m -> m == [ Green , Blue ]
      _     -> False

   |] >>= runIO . configurable [ImportPrelude] . compile 
  )

color 0 = constructors [ Just [], Just [], Just [], Nothing ]
color i = constructors [ Just [], Just [], Just [], Just [ uList 3 (color (i-1)) ] ]

allocator = color 1

result = solveAndTestBoolean allocator encMain main 
