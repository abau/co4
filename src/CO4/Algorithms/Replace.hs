{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.Replace
  (replaceExpressions, replaceExpression)
where

import           Control.Monad.Reader
import           Control.Applicative (Applicative)
import qualified Data.Map as M
import           CO4.Algorithms.Instantiator
import           CO4.Language

type Mapping = M.Map Expression Expression

newtype Replacer a = Replacer { runReplacer :: Reader Mapping a }
  deriving (Functor, Applicative, Monad, MonadReader Mapping)

instance MonadInstantiator Replacer where
  instantiateExpression exp = do
    image <- asks $ M.lookup exp
    case image of
      Just exp' -> return exp'
      Nothing   -> instantiateSubexpressions exp

replaceExpressions :: Instantiable a => [(Expression,Expression)] -> a -> a
replaceExpressions replacements a = 
  runReader (runReplacer $ instantiate a) $ M.fromList replacements

replaceExpression :: Instantiable a => Expression -> Expression -> a -> a
replaceExpression old new = replaceExpressions [(old,new)]
