-- |Separated data definition for breaking mutually recursive module cycles
module CO4.EncodedAdtData
  (Primitive, EncodedAdt (..))
where

import           Satchmo.Core.Boolean (Boolean)

type Primitive = Boolean

data EncodedAdt = EncodedAdt { _id          :: Int
                             , _definedness :: Primitive
                             , _flags       :: [Primitive] 
                             , _arguments   :: [EncodedAdt] 
                             }
                | Bottom

instance Eq EncodedAdt where
  Bottom == Bottom = True
  _      == Bottom = False
  Bottom == _      = False
  a      == b      = _id a == _id b

instance Ord EncodedAdt where
  compare Bottom Bottom = EQ
  compare _      Bottom = GT
  compare Bottom _      = LT
  compare a      b      = compare (_id a) (_id b)
