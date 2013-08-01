module CO4.EncodedAdt
  (EncodedAdt,Primitive,makeWithStackTrace)
where

import Satchmo.Core.Boolean (Boolean)
import CO4.Stack (StackTrace)

type Primitive = Boolean
data EncodedAdt

instance Eq  EncodedAdt
instance Ord EncodedAdt

makeWithStackTrace :: Int -> Primitive -> [Primitive] -> [EncodedAdt] -> StackTrace
                   -> EncodedAdt
