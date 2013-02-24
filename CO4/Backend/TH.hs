{-# LANGUAGE FlexibleInstances #-}
module CO4.Backend.TH
  (module CO4.Backend)
where

import           Control.Monad.Identity (runIdentity)
import qualified Language.Haskell.TH as TH
import           CO4.Backend
import           CO4.Algorithms.THInstantiator

instance ProgramBackend [TH.Dec] where
  displayProgram = runIdentity . instantiate

instance ExpressionBackend TH.Exp where
  displayExpression = runIdentity . instantiate

instance SchemeBackend TH.Type where
  displayScheme = runIdentity . instantiate
