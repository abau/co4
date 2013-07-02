module CO4.EncEq 
  (EncEq (..), encEq)
where

import CO4.EncodedAdt (Primitive,EncodedAdt,make)
import CO4.Monad (CO4)

class EncEq a where
  encEqPrimitive :: a -> EncodedAdt -> EncodedAdt -> CO4 Primitive

encEq :: (EncEq a) => a -> EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encEq a b c = encEqPrimitive a b c >>= \p -> make [p] []
