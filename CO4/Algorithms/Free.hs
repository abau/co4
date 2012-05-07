module CO4.Algorithms.Free
  (Free (..))
where

import           Data.List ((\\),delete,nub)
import           CO4.Algorithms.Bound (bound)
import           CO4.Language

class Free a where
  -- |@free e@ returns the list of names, that appear free in @e@
  free :: a -> [Name]

instance Free Type where
  free (TVar v)    = [v]
  free (TCon _ ts) = nub $ concatMap free ts

instance Free Scheme where
  free (SType t)          = free t
  free (SForall n scheme) = delete n $ free scheme

instance Free Expression where
  free (EVar v)      = [v]
  free (ECon _)      = []
  free (ELit _)      = []
  free (EApp f args) = nub $ concatMap free (f:args)
  free (ETApp f _)   = free f
  free (ELam ns e)   = free e \\ ns
  free (ETLam _ e)   = free e
  free (ECase e ms)  = nub $ free e ++ (concatMap free ms)
  free (ELet n v e)  = delete n $ nub $ free v ++ (free e)

instance Free Match where
  free (Match pat exp) = free exp \\ (bound pat)
