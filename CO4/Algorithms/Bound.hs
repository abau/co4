module CO4.Algorithms.Bound
  (Bound (..))
where

import           Data.List (nub)
import           CO4.Language
import           CO4.Names (name)

class Bound a where
  bound :: a -> [Name]

instance Bound Pattern where
  bound (PVar n)    = [n]
  bound (PLit _)    = []
  bound (PCon _ ps) = nub $ concatMap bound ps

instance Bound Scheme where
  bound (SType _)     = []
  bound (SForall n s) = (name n) : bound s
