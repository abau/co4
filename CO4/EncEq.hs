{-# LANGUAGE MultiParamTypeClasses #-}
module CO4.EncEq 
  (EncEq (..))
where

import Satchmo.Core.MonadSAT (MonadSAT)
import Satchmo.Core.Primitive (Primitive)
import CO4.EncodedAdt (EncodedAdt)
import CO4.Cache (MonadCache)
import CO4.Profiling (MonadProfiling)

class (Primitive p, EncodedAdt e p) => EncEq a e p where
  encEq :: (MonadSAT m, MonadCache (e p) m, MonadProfiling m) 
        => a -> e p -> e p -> m (e p)
