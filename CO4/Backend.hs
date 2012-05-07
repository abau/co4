{-# LANGUAGE FlexibleInstances #-}
-- |Backend class for transforming intermediate language into other languages
module CO4.Backend
  (ProgramBackend (..), ExpressionBackend (..), SchemeBackend (..))
where

import CO4.Language (Program,Expression,Scheme)
import CO4.PPrint (pprint)

class ProgramBackend a where
  displayProgram    :: Program -> a

class ExpressionBackend a where
  displayExpression :: Expression -> a

class SchemeBackend a where
  displayScheme     :: Scheme -> a

instance ProgramBackend String where
  displayProgram = show . pprint

instance ExpressionBackend String where
  displayExpression = show . pprint

instance SchemeBackend String where
  displayScheme = show . pprint
