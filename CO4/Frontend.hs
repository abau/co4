-- |Frontend class for parsing intermediate language from other languages
module CO4.Frontend
  (ProgramFrontend (..), ExpressionFrontend (..), SchemeFrontend (..))
where

import CO4.Language (Program,Expression,Scheme)
import CO4.Unique (MonadUnique)

class ProgramFrontend a where
  parseProgram             :: a ->        Program
  parsePreprocessedProgram :: MonadUnique u => a -> u Program
  parsePreprocessedProgram = return . parseProgram

class ExpressionFrontend a where
  parseExpression             :: a ->        Expression
  parsePreprocessedExpression :: MonadUnique u => a -> u Expression
  parsePreprocessedExpression = return . parseExpression

class SchemeFrontend a where
  parseScheme     :: a -> Scheme
