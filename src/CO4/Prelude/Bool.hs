module CO4.Prelude.Bool
  ( xor2
  , encAnd2, encAnd, encOr2, encOr, encNot, encXor2
  , encAnd2Prof, encAndProf, encOr2Prof, encOrProf, encNotProf, encXor2Prof
  )
where

import           Prelude hiding (not,and,or)
import           CO4.EncodedAdt 
import           CO4.Monad (CO4,traced)
import           CO4.Prelude.Nat (onFlags,catchInvalid,onFlags2,catchInvalid2)
import           Satchmo.Core.Primitive (not,and,or,xor,constant)

encAnd2,encAnd2Prof :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encAnd2 = catchInvalid2 $ onFlags2 $ \[a] [b] -> do
  r <- and [a,b]
  return [r]
encAnd2Prof a b = traced "and2" $ encAnd2 a b

encAnd,encAndProf :: EncodedAdt -> CO4 EncodedAdt
encAnd xs = do 
  c <- make (constant True) [constant True] [] True
  foldList encAnd2 c and xs
encAndProf = traced "and" . encAnd

encOr2,encOr2Prof :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encOr2 = catchInvalid2 $ onFlags2 $ \[a] [b] -> do
  r <- or [a,b]
  return [r] 
encOr2Prof a b = traced "or2" $ encOr2 a b

encOr,encOrProf :: EncodedAdt -> CO4 EncodedAdt
encOr xs = do 
  c <- make (constant True) [constant False] [] True
  foldList encOr2 c or xs
encOrProf = traced "or" . encOr

encNot,encNotProf :: EncodedAdt -> CO4 EncodedAdt
encNot = catchInvalid $ onFlags $ \[a] -> return [not a]
encNotProf a = traced "not" $ encNot a

xor2 :: Bool -> Bool -> Bool
xor2 = (/=)

encXor2,encXor2Prof :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encXor2 = catchInvalid2 $ onFlags2 $ \[a] [b] -> do
  r <- xor [a,b]
  return [r] 
encXor2Prof a b = traced "xor2" $ encXor2 a b

-- |`foldConstantList f c fConst xs` folds a list of Booleans `xs`. 
-- If each definedness flag in `xs` is constantly true and each list constructor is constant,
-- then all Booleans in `xs` are folded by `fConst`.
-- Otherwise `xs` is folded by `encFoldList f c xs`.
foldList :: (EncodedAdt -> EncodedAdt -> CO4 EncodedAdt) -> EncodedAdt 
         -> ([Primitive] -> CO4 Primitive)
         -> EncodedAdt
         -> CO4 EncodedAdt
foldList f c fConst xs = 
  case boolPrimitives xs of
    Nothing -> encFoldList f c xs
    Just ps -> do p <- fConst ps
                  make (constant True) [p] [] True

encFoldList :: (EncodedAdt -> EncodedAdt -> CO4 EncodedAdt) 
            -> EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encFoldList f c xs = onValidDiscriminant xs 2 $ do 
  nilBranch  <- ifReachable xs 0 2 $ return c
  consBranch <- ifReachable xs 1 2 $
    let y  = constructorArgument 2 0 1 xs
        ys = constructorArgument 2 1 1 xs
    in do
     acc <- f c y
     encFoldList f acc ys;
  caseOf xs [nilBranch, consBranch]

-- |Returns Boolean primitives of a list of Booleans `xs`, if each definedness flag
-- in `xs` is constantly true and each list constructor is constant.
boolPrimitives :: EncodedAdt -> Maybe [Primitive]
boolPrimitives xs = if isEmpty xs then Just [] else
  if isConstantlyDefined xs
  then case constantConstructorIndex 2 xs of
    Nothing -> Nothing
    Just 0  -> Just []
    Just i  -> let args = arguments' xs
                   y    = args !! 0
                   ys   = args !! 1
               in
                 if isConstantlyDefined y
                 then fmap ((head $ flags' y) :) $ boolPrimitives ys
                 else Nothing
  else Nothing
