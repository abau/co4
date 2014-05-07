module CO4.EncodedAdt
  (EncodedAdt,Primitive,makeWithId)
where

import Satchmo.Core.Boolean (Boolean)

type Primitive = Boolean
data EncodedAdt

instance Eq  EncodedAdt
instance Ord EncodedAdt

makeWithId :: Int -> Primitive -> [Primitive] -> [EncodedAdt] -> Bool -> EncodedAdt
