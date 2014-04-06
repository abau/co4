module CO4.EncodedAdt
  (EncodedAdt,Primitive,makeWithStackTrace)
where

import Satchmo.Core.Boolean (Boolean)
import CO4.Stack (CallStackTrace)

type Primitive = Boolean
data EncodedAdt

instance Eq  EncodedAdt
instance Ord EncodedAdt

makeWithStackTrace :: Int -> Primitive -> [Primitive] -> [EncodedAdt] -> CallStackTrace
                   -> EncodedAdt
