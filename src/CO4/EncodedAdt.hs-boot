module CO4.EncodedAdt
  (EncodedAdt,Primitive,makeWithId,constantConstructorIndex)
where

import Satchmo.Core.Boolean (Boolean)

type Primitive = Boolean
data EncodedAdt

instance Eq  EncodedAdt
instance Ord EncodedAdt

makeWithId :: Int -> Primitive -> [Primitive] -> [EncodedAdt] -> Bool -> EncodedAdt

constantConstructorIndex :: Int -> EncodedAdt -> Maybe Int
