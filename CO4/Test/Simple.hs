{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

module CO4.Test.Simple
where

import           Prelude (undefined,(>>=),error,Show (..),putStrLn,(.))
import qualified GHC.Types
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Algorithms.Eitherize.UnsizedAdt (UnsizedAdt)

$( [d| data Bool = False | True

       main x = case x of False -> False
                          True  -> True

   |] >>= runIO . configurable [Verbose] . compile 
  )

result = solveAndTestBoolean GHC.Types.True (undefined :: SizedBool) encMain main
