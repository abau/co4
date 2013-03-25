{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CO4.Test.Simple
where

import           Prelude (undefined,(>>=),error,Show (..),putStrLn,(.))
import           Data.Maybe
import qualified GHC.Types
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4

$( [d| data Bool = False | True

       main x = case x of False -> False
                          True  -> True

   |] >>= runIO . configurable [Verbose] . compile 
  )

allocator = allocate ( constructors [ Just [] , Just [] ] )

result = solveAndTestBoolean GHC.Types.True allocator encMain main
