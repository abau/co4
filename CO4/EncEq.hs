module CO4.EncEq 
  (EncEq (..), encEq)
where

import Prelude hiding (and)
import CO4.EncodedAdt (Primitive,EncodedAdt,make,definedness)
import Satchmo.Core.Primitive (and)
import CO4.Monad (CO4)

class EncEq a where
  encEqPrimitive :: a -> EncodedAdt -> EncodedAdt -> CO4 Primitive

encEq :: (EncEq a) => a -> EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encEq type_ a b = encEqPrimitive type_ a b >>= \p -> do 
  definedness' <- and [definedness a, definedness b]
  make definedness' [p] []
