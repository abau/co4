module CO4.Encodeable
  (Encodeable (..))
where

import Satchmo.Core.MonadSAT (MonadSAT)
import Satchmo.Core.Primitive (Primitive)
import CO4.EncodedAdt (EncodedAdt)

class Encodeable a where
  encode :: (MonadSAT m, Primitive p) => a -> m (EncodedAdt p)
