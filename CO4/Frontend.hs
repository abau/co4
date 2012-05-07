-- |Frontend class for parsing intermediate language from other languages
module CO4.Frontend
  (ProgramFrontend (..), ExpressionFrontend (..), SchemeFrontend (..))
where

import CO4.Language (Program,Expression,Scheme)
import CO4.Unique (Unique)

class ProgramFrontend a where
  parseProgram    :: a -> Unique Program

class ExpressionFrontend a where
  parseExpression :: a -> Unique Expression

class SchemeFrontend a where
  parseScheme     :: a -> Scheme
