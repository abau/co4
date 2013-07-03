{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
module CO4.PreludeNat 
  (Nat8, nat8, uNat8, kNat8
  , gtNat8, geNat8, eqNat8, leNat8, ltNat8, maxNat8, minNat8, plusNat8, timesNat8
  , encNat8, encGtNat8, encGeNat8, encEqNat8, encLeNat8, encLtNat8
  , encMaxNat8, encMinNat8, encPlusNat8, encTimesNat8
  , encNat8Prof, encGtNat8Prof, encGeNat8Prof, encEqNat8Prof, encLeNat8Prof
  , encLtNat8Prof, encMaxNat8Prof, encMinNat8Prof, encPlusNat8Prof, encTimesNat8Prof
  )
where

import           Prelude hiding (not,and,or)
import           Control.Monad (zipWithM,forM)
import           Data.Word (Word8)
import qualified Data.Map as M
import           Satchmo.Core.Decode (Decode,decode)
import           Satchmo.Core.Primitive 
  (primitive,constant,assert,not,and,xor,or,equals)
import           CO4.Monad (CO4,SAT,traced)
import           CO4.EncodedAdt hiding (undefined)
import           CO4.Encodeable (Encodeable (..))
import           CO4.AllocatorData (Allocator,known,constructors)
import           CO4.EncEq (EncEq(..))

type Nat8 = Word8

instance Encodeable Nat8 where
  encode i = encodedConstructor (fromIntegral i) (2^8) []

instance Decode SAT EncodedAdt Nat8 where
  decode p = toIntermediateAdt p (2^8) >>= \case 
    IntermediateUndefined -> error $ "Can not decode 'undefined' to data of type 'Nat8'"
    IntermediateConstructorIndex i _ -> return $ fromIntegral i

instance EncEq Nat8 where
  encEqPrimitive _ a b = encEqNat8 a b >>= return . head . flags'

uNat8 :: Allocator
uNat8 = constructors $ replicate (2^8) $ Just []

kNat8 :: Int -> Allocator
kNat8 i = known i (2^8) []

-- * Plain functions on naturals

nat8 :: Int -> Nat8
nat8 = fromIntegral

gtNat8,geNat8,eqNat8,leNat8,ltNat8 :: Nat8 -> Nat8 -> Bool
gtNat8 = (>)
geNat8 = (>=)
eqNat8 = (==)
leNat8 = (<=)
ltNat8 = (<)

maxNat8,minNat8,plusNat8,timesNat8 :: Nat8 -> Nat8 -> Nat8
maxNat8   = max
minNat8   = min
plusNat8  = (+)
timesNat8 = (*)

-- * Encoded functions on naturals

encNat8,encNat8Prof  :: Int -> CO4 EncodedAdt
encNat8 i = encodedConstructor i (2^8) []
encNat8Prof = traced "nat8" . encNat8

encGtNat8,encGeNat8,encEqNat8,encLeNat8,encLtNat8,encMaxNat8,encMinNat8
  ,encGtNat8Prof,encGeNat8Prof,encEqNat8Prof,encLeNat8Prof,encLtNat8Prof
  ,encMaxNat8Prof,encMinNat8Prof :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encGtNat8 = flip encLtNat8
encGtNat8Prof a b = traced "gtNat8" $ encGtNat8 a b

encGeNat8 = flip encLeNat8
encGeNat8Prof a b = traced "geNat8" $ encGeNat8 a b

encEqNat8 = catchInvalid2 $ onFlags 
  (\as bs -> zipWithM (\x y -> equals [x,y]) as bs >>= and 
                                                   >>= \r -> make [r] [])
encEqNat8Prof a b = traced "eqNat8" $ encEqNat8 a b

encLeNat8 = catchInvalid2 $ onFlags $ \a b -> do
  (l, e) <- encComparePrimitives a b
  r <- or [l,e] 
  make [r] []
encLeNat8Prof a b = traced "leNat8" $ encLeNat8 a b

encLtNat8 = catchInvalid2 $ onFlags $ \a b -> do
  (l, _) <- encComparePrimitives a b
  make [l] []
encLtNat8Prof a b = traced "ltNat8" $ encLtNat8 a b

encMaxNat8 = catchInvalid2 $ onFlags $ \ a b -> do
  (l, _) <- encComparePrimitives b a
  r      <- zipWithM ( \x y -> ifthenelse l x y ) a b 
  make r []
encMaxNat8Prof a b = traced "maxNat8" $ encMaxNat8 a b

encMinNat8 = catchInvalid2 $ onFlags $ \ a b -> do
  (l, _) <- encComparePrimitives a b
  r      <- zipWithM ( \x y -> ifthenelse l x y ) a b 
  make r []
encMinNat8Prof a b = traced "minNat8" $ encMinNat8 a b

encComparePrimitives :: [Primitive] -> [Primitive] 
                     -> CO4 (Primitive, Primitive) -- ^ (less, equals)
encComparePrimitives a b = case (a,b) of
  ([],[]) -> return ( constant False, constant True )
  ((x:xs),(y:ys)) -> do
    ( ll, ee ) <- encComparePrimitives xs ys
    l <- primitive ; e <- primitive

    let implies xs ys = assert (map not xs ++ ys)

    --   l <-> ( ll || (ee && (x < y)) )
    implies [ ll ] [ l ]
    implies [ ee, not x, y ] [ l ]
    implies [ l ] [ ll, ee ]
    implies [ l ] [ ll, not x ]
    implies [ l ] [ ll, y ]

    --   e <->   (   ee && (x == y) )
    implies [ ee, not x, not y ] [ e ]
    implies [ ee, x, y ] [ e ]
    implies [ not ee ] [ not e ]
    implies [ x, not y ] [ not e ]
    implies [ not x, y ] [ not e ]

    return ( l, e )
{-
  (xs, []) -> do
    x <- or xs
    return (constant False, not x )
  ([], ys) -> do
    y <- or ys
    return ( y, not y )
-}

encPlusNat8,encPlusNat8Prof :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encPlusNat8 = catchInvalid2 $ onFlags $ \ (a:as) (b:bs) -> do
  (z,c) <- halfAdder a b
  zs <- addWithCarry c as bs
  make (z : zs) []
  where
    addWithCarry c [] [] = do
        assert [ not c ] 
        return []
    addWithCarry c ( x : xs) ( y:ys ) = do
          (z,d) <- fullAdder c x y
          zs <- addWithCarry d xs ys
          return $ z : zs
encPlusNat8Prof a b = traced "plusNat8" $ encPlusNat8 a b

encTimesNat8 :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encTimesNat8 = catchInvalid2 $ onFlags $ \as bs -> do
  kzs <- product_components (Just 8) as bs
  export (Just 8) kzs

  where
    product_components bound as bs = sequence $ do
        ( i , x ) <- zip [ 0 .. ] as
        ( j , y ) <- zip [ 0 .. ] bs
        return $ do
            z <- and [ x, y ]
            if ( case bound of Nothing -> False ; Just b -> i+j >= b )
                 then do assert [ not z ] ; return ( i+j , [ ] )
                 else do return ( i+j , [z] )

    export bound kzs = do
        m <- reduce bound $ M.fromListWith (++) kzs
        case M.maxViewWithKey m of
            Nothing -> make [] []
            Just ((k,_) , _) -> do
                  make (do i <- [ 0 .. k ]
                           let { [ b ] = m M.! i }
                           return b
                       ) []

    reduce bound m = case M.minViewWithKey m of
        Nothing -> return M.empty
        Just ((k, bs), rest ) ->
            if ( case bound of Nothing -> False ; Just b -> k >= b )
            then do
                forM bs $ \ b -> assert [ not b ]
                reduce bound rest
            else case bs of
                [] -> reduce bound rest
                [x] -> do
                    m' <- reduce bound rest
                    return $ M.unionWith (error "PreludeNat.encTimesNat8: huh") m'
                           $ M.fromList [(k,[x])]
                [x,y] -> do
                    (r,c) <- halfAdder x y
                    reduce bound $ M.unionWith (++) rest
                           $ M.fromList [ (k,[r]), (k+1, [c]) ]
                (x:y:z:more) -> do
                    (r,c) <- fullAdder x y z
                    reduce bound $ M.unionWith (++) rest
                           $ M.fromList [ (k, more ++ [r]), (k+1, [c]) ]
encTimesNat8Prof a b = traced "timesNat8" $ encTimesNat8 a b

ifthenelse :: Primitive -> Primitive -> Primitive -> CO4 Primitive
ifthenelse i t e = do
    r <- primitive
    assert [ not i , not t, r ]
    assert [ not i , not r, t ]
    assert [     i , not e, r ]
    assert [     i , not r, e ]
    return r

fullAdder :: Primitive -> Primitive -> Primitive -> CO4 (Primitive,Primitive)
fullAdder = fullAdder_three

{-
fullAdder_one p1 p2 p3 = do
  (r12,c12) <- halfAdder p1 p2
  (r,c3) <- halfAdder r12 p3
  c <- or [c12,c3]
  return (r, c)
-}

fullAdder_three x y z = do
    let implies xs ys = assert (map not xs ++ ys)
    r <- primitive
    implies [ not x, not y, not z ] [ not r ]
    implies [ not x, not y,     z ] [     r ]
    implies [ not x,     y, not z ] [     r ]
    implies [ not x,     y,     z ] [ not r ]
    implies [     x, not y, not z ] [     r ]
    implies [     x, not y,     z ] [ not r ]
    implies [     x,     y, not z ] [ not r ]
    implies [     x,     y,     z ] [     r ]
    c <- primitive
    implies [ x, y ] [ c ]
    implies [ y, z ] [ c ]
    implies [ z, x ] [ c ] 
    implies [ not x, not y ] [ not c ]
    implies [ not y, not z ] [ not c ]
    implies [ not z, not x ] [ not c ]
    return (r,c)

{-
fullAdder_two p1 p2 p3 = do
  p4 <- primitive
  p5 <- primitive
  assert [not p1, not p2, p5]
  assert [not p1, not p3, p5]
  assert [not p1, p4, p5]
  assert [p1, p2, not p5]
  assert [p1, p3, not p5]
  assert [p1, not p4, not p5]
  assert [not p2, not p3, p5]
  assert [not p2, p4, p5]
  assert [p2, p3, not p5]
  assert [p2, not p4, not p5]
  assert [not p3, p4, p5]
  assert [p3, not p4, not p5]
  assert [not p1, not p2, not p3, p4]
  assert [not p1, not p2, p3, not p4]
  assert [not p1, p2, not p3, not p4]
  assert [not p1, p2, p3, p4]
  assert [p1, not p2, not p3, not p4]
  assert [p1, not p2, p3, p4]
  assert [p1, p2, not p3, p4]
  assert [p1, p2, p3, not p4]
  return ( p4, p5 )
-}

halfAdder :: Primitive -> Primitive -> CO4 (Primitive,Primitive)
halfAdder p1 p2 = do
  c <- and [ p1, p2 ]
  r <- xor [ p1, p2 ]
  return (r,c)

onFlags :: ([Primitive] -> [Primitive] -> CO4 a) -> EncodedAdt -> EncodedAdt -> CO4 a
onFlags f a b = case (flags a, flags b) of
  (Just as, Just bs) -> f as bs
  _                  -> error "PreludeNat.onFlags: missing flags"

catchInvalid2 :: (EncodedAdt -> EncodedAdt -> CO4 (EncodedAdt)) 
              -> EncodedAdt -> EncodedAdt -> CO4 (EncodedAdt)
catchInvalid2 f a b = 
  if isConstantlyUndefined a || isConstantlyUndefined b
  then return undefined
  else if isBottom a || isBottom b 
       then return bottom
       else f a b
