-- |Separated data definition for breaking mutually recursive module cycles
module CO4.EncodedAdtData
  (Primitive, EncodedAdt (..), makeWithStackTrace)
where

import           Text.PrettyPrint (Doc,vcat,text)
import           Satchmo.Core.Boolean (Boolean)
import           CO4.Stack (StackTrace)

type Primitive = Boolean

data EncodedAdt = EncodedAdt { _id          :: ! Int
                             , _definedness :: ! Primitive
                             , _flags       :: ! [Primitive] 
                             , _arguments   :: ! [EncodedAdt] 
                             , _origin      :: ! Doc
                             }
                | Empty

makeWithStackTrace :: Int -> Primitive -> [Primitive] -> [EncodedAdt] -> StackTrace
                   -> EncodedAdt
makeWithStackTrace i d f a o = EncodedAdt i d f a $ vcat $ map text o

instance Eq EncodedAdt where
  Empty == Empty = True
  _     == Empty = False
  Empty == _     = False
  a     == b     = _id a == _id b

instance Ord EncodedAdt where
  compare Empty  Empty  = EQ
  compare _      Empty  = GT
  compare Empty  _      = LT
  compare a      b      = compare (_id a) (_id b)
