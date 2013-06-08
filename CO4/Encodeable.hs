{-# LANGUAGE MultiParamTypeClasses #-}
module CO4.Encodeable
  (Encodeable (..))
where

import Satchmo.Core.MonadSAT (MonadSAT)
import Satchmo.Core.Primitive (Primitive)
import CO4.EncodedAdt (EncodedAdt)

class (Primitive p, EncodedAdt e p) => Encodeable a e p where
  encodeConstant :: a -> e p

  encode :: (MonadSAT m) => a -> m (e p)
  encode = return . encodeConstant
