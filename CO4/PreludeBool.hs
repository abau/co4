module CO4.PreludeBool
  ( xor2
  , encAnd2, encOr2, encNot, encXor2
  , encAnd2Prof, encOr2Prof, encNotProf, encXor2Prof
  )
where

import           Prelude hiding (not,and,or)
import           CO4.EncodedAdt 
import           CO4.Monad (CO4,traced)
import           CO4.PreludeNat (onFlags,catchInvalid,onFlags2,catchInvalid2)
import           Satchmo.Core.Primitive (not,and,or,xor)

encAnd2,encAnd2Prof :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encAnd2 = catchInvalid2 $ onFlags2 $ \[a] [b] -> do
  r <- and [a,b]
  make [r] []
encAnd2Prof a b = traced "and2" $ encAnd2 a b

encOr2,encOr2Prof :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encOr2 = catchInvalid2 $ onFlags2 $ \[a] [b] -> do
  r <- or [a,b]
  make [r] []
encOr2Prof a b = traced "or2" $ encOr2 a b

encNot,encNotProf :: EncodedAdt -> CO4 EncodedAdt
encNot = catchInvalid $ onFlags $ \[a] -> do
  make [not a] []
encNotProf a = traced "not" $ encNot a

xor2 :: Bool -> Bool -> Bool
xor2 = (/=)

encXor2,encXor2Prof :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encXor2 = catchInvalid2 $ onFlags2 $ \[a] [b] -> do
  r <- xor [a,b]
  make [r] []
encXor2Prof a b = traced "xor2" $ encXor2 a b

