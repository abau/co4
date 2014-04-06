module CO4.Encodeable
  (Encodeable (..))
where

import CO4.EncodedAdt (EncodedAdt)
import CO4.Monad (CO4)

class Encodeable a where
  encode :: a -> CO4 EncodedAdt
