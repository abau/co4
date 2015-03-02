module CO4.Prelude.Bool
  ( xor2
  , encAnd2, encAnd, encOr2, encOr, encNot, encXor2
  , encAnd2Prof, encAndProf, encOr2Prof, encOrProf, encNotProf, encXor2Prof
  )
where

import           Prelude hiding (not,and,or)
import           Satchmo.Core.Primitive (not,and,or,xor,constant)
import           CO4.EncodedAdt 
import           CO4.Monad (CO4,traced)
import           CO4.Prelude.Nat (onUnwrappedFlags1,onUnwrappedFlags2)
import           CO4.Algorithms.UndefinedValues.Data 
  (wrappedValue,withDefinedness,encCO4OVUndefinedCons,encCO4OVDefinedCons)

encAnd2,encAnd2Prof :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encAnd2 = onUnwrappedFlags2 $ \[a] [b] -> do
  r <- and [a,b]
  return (constant True, [r])
encAnd2Prof a b = traced "and2" $ encAnd2 a b

encAnd,encAndProf :: EncodedAdt -> CO4 EncodedAdt
encAnd xs = do 
  c <- make [constant True] [] True >>= encCO4OVDefinedCons
  foldList encAnd2 c and xs
encAndProf = traced "and" . encAnd

encOr2,encOr2Prof :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encOr2 = onUnwrappedFlags2 $ \[a] [b] -> do
  r <- or [a,b]
  return (constant True, [r])
encOr2Prof a b = traced "or2" $ encOr2 a b

encOr,encOrProf :: EncodedAdt -> CO4 EncodedAdt
encOr xs = do 
  c <- make [constant False] [] True >>= encCO4OVDefinedCons
  foldList encOr2 c or xs
encOrProf = traced "or" . encOr

encNot,encNotProf :: EncodedAdt -> CO4 EncodedAdt
encNot = onUnwrappedFlags1 $ \[a] -> return (constant True, [not a])
encNotProf a = traced "not" $ encNot a

xor2 :: Bool -> Bool -> Bool
xor2 = (/=)

encXor2,encXor2Prof :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encXor2 = onUnwrappedFlags2 $ \[a] [b] -> do
  r <- xor [a,b]
  return (constant True, [r])
encXor2Prof a b = traced "xor2" $ encXor2 a b

-- |`foldConstantList f c fConst xs` folds a list of Booleans `xs`. 
-- If each list constructor is constant,
-- then all Booleans in `xs` are folded by `fConst`.
-- Otherwise `xs` is folded by `encFoldList f c xs`.
foldList :: (EncodedAdt -> EncodedAdt -> CO4 EncodedAdt) -> EncodedAdt 
         -> ([Primitive] -> CO4 Primitive)
         -> EncodedAdt
         -> CO4 EncodedAdt
foldList f c fConst wrapped = 
  case wrappedValue wrapped of
    Nothing -> encCO4OVUndefinedCons
    Just (unwrapped, defs) -> case boolPrimitives unwrapped of
      Just ps -> do 
        def <- and defs
        p   <- fConst ps
        make [p] [] True >>= withDefinedness def

      Nothing -> encFoldList f c wrapped

encFoldList :: (EncodedAdt -> EncodedAdt -> CO4 EncodedAdt) 
            -> EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encFoldList f c wrapped = onValidDiscriminant wrapped 2 $ do 
  undefBranch <- ifReachable wrapped 0 2 encCO4OVUndefinedCons
  defBranch   <- ifReachable wrapped 1 2 $
    let unwrapped = constructorArgument 2 0 1 wrapped
    in
      onValidDiscriminant unwrapped 2 $ do 
        nilBranch  <- ifReachable unwrapped 0 2 $ return c
        consBranch <- ifReachable unwrapped 1 2 $
          let y  = constructorArgument 2 0 1 unwrapped
              ys = constructorArgument 2 1 1 unwrapped
          in do
           acc <- f c y
           encFoldList f acc ys;
        caseOf unwrapped [nilBranch, consBranch]
  caseOf wrapped [undefBranch, defBranch]

-- |Returns Boolean primitives of a list of Booleans `xs`, if each list constructor is constant.
boolPrimitives :: EncodedAdt -> Maybe [Primitive]
boolPrimitives xs = if isEmpty xs then Just [] else
  case constantConstructorIndex 2 xs of
    Nothing -> Nothing
    Just 0  -> Just []
    Just 1  -> let args = arguments' xs
                   y    = args !! 0
                   ys   = args !! 1
               in
                 fmap ((head $ flags' y) :) $ boolPrimitives ys
