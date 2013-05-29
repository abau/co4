{-# LANGUAGE ScopedTypeVariables #-}
module CO4.EncEq 
  (EncEq (..))
where

import Satchmo.Core.MonadSAT (MonadSAT)
import Satchmo.Core.Primitive (Primitive)
import CO4.EncodedAdt (EncodedAdt)
import CO4.Cache (MonadCache)

class EncEq a where
  encEq :: (MonadSAT m,Primitive p,MonadCache p m) 
        => a -> EncodedAdt p -> EncodedAdt p -> m (EncodedAdt p)
