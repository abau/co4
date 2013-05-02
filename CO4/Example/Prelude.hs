{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CO4.Example.Prelude
where

import           Prelude (undefined,(>>=),error,Show (..),putStrLn,(.),(-))
import qualified Data.Maybe as M
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4

$(prelude)

$( [d| 

    main xs = and2 ( eqUnary uTen   (sum xs   ) )
                   ( eqUnary uThree (length xs) )

   |] >>= runIO . configurable [ImportPrelude] . compile 
  )

uUnary 0 = constructors [ M.Just [] , M.Nothing ]
uUnary i = constructors [ M.Just [] , M.Just [ uUnary (i-1) ] ]

uList 0 _ = constructors [ M.Just [] , M.Nothing ]
uList i a = constructors [ M.Just [] , M.Just [ a, uList (i-1) a ] ]

allocator = uList 10 (uUnary 10)

result = solveAndTestBoolean allocator encMain main 
