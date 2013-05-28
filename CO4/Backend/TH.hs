{-# LANGUAGE FlexibleInstances #-}
module CO4.Backend.TH
  (module CO4.Backend)
where

import qualified Language.Haskell.TH as TH
import           CO4.Backend
import           CO4.Algorithms.THInstantiator (toTH)

instance ProgramBackend [TH.Dec] where
  displayProgram = toTH

instance ExpressionBackend TH.Exp where
  displayExpression = toTH

instance SchemeBackend TH.Type where
  displayScheme = toTH
