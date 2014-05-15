{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module CO4.PreludeNat 
  ( Nat, width, value, nat, uNat, knownNat
  , gtNat, geNat, eqNat, leNat, ltNat
  , isZeroNat
  , maxNat, minNat, timesNat
  , plusNat, plus'Nat, plusCLANat, plus'CLANat
  , shiftLNat, shiftRNat, andNat, orNat, xorNat

  , encNat, encGtNat, encGeNat, encEqNat, encLeNat, encLtNat
  , encIsZeroNat
  , encMaxNat, encMinNat, encTimesNat
  , encPlusNat, encPlus'Nat, encPlusCLANat, encPlus'CLANat
  , encShiftLNat, encShiftRNat, encAndNat, encOrNat, encXorNat

  , encNatProf
  , encGtNatProf, encGeNatProf, encEqNatProf, encLeNatProf, encLtNatProf
  , encIsZeroNatProf
  , encMaxNatProf, encMinNatProf, encTimesNatProf
  , encPlusNatProf, encPlus'NatProf, encPlusCLANatProf, encPlus'CLANatProf
  , encShiftLNatProf, encShiftRNatProf, encAndNatProf, encOrNatProf, encXorNatProf

  , onFlags, catchInvalid, onFlags2, catchInvalid2
  )
where

import           Prelude hiding (not,and,or,abs)
import qualified Prelude
import qualified Control.Exception as Exception
import           Control.Monad (zipWithM,forM, when)
import           Data.Bits ((.&.),(.|.))
import qualified Data.Bits as B
import           Data.Function (on)
import qualified Data.Map.Strict as M
import           Satchmo.Core.Decode (Decode,decode)
import           Satchmo.Core.Primitive 
  (primitive,constant,assert,not,and,xor,or,equals)
import           CO4.Monad (CO4,SAT,traced,abortWithStackTrace)
import           CO4.EncodedAdt 
import           CO4.Encodeable (Encodeable (..))
import           CO4.Allocator
import           CO4.Util (toBinary,fromBinary,bitWidth)

--import qualified CO4.PreludeNat.Opt as Opt

data Nat = Nat { width :: Int
               , value :: Integer
               }

instance Eq Nat where
  (==) = eqNat

instance Ord Nat where
  compare = compare `on` value

instance Show Nat where
  show x = concat ["(nat ", show $ width x, " ", show $ value x, ")"]

instance Encodeable Nat where
  encode n = encNat (width n) (value n)

instance Decode SAT EncodedAdt Nat where
  decode p = case fmap length (flags p) of
    Just n -> decode (definedness p) >>= \case
      False -> error $ "Can not decode 'undefined' to data of type 'Nat'"
      True  -> decode (flags' p) >>= \case
        [] | n == 0 -> return $ nat 0 0
        fs          -> return $ nat n $ fromBinary fs
    Nothing -> error "Missing flags while decoding 'Nat'"

instance FromKnown Nat where
  fromKnown n = knownNat (width n) (value n)

instance Complete Nat where
  complete = uNat maxBound

uNat :: Int -> TAllocator Nat
uNat = unsafeTAllocator . BuiltInUnknown

knownNat :: Int -> Integer -> TAllocator Nat
knownNat 0 0 = unsafeTAllocator $ BuiltInKnown []
knownNat w i = unsafeTAllocator $ BuiltInKnown $ toBinary (Just w) i 

-- * Plain functions on naturals

nat :: Int -> Integer -> Nat
nat w n = Exception.assert (w >= (bitWidth $ n + 1)) $ Nat w n

gtNat,geNat,eqNat,leNat,ltNat :: Nat -> Nat -> Bool
gtNat = onValue2' (>)
geNat = onValue2' (>=)
eqNat = onValue2' (==)
leNat = onValue2' (<=)
ltNat = onValue2' (<)

isZeroNat :: Nat -> Bool
isZeroNat n = 0 == value n

maxNat,minNat,plusNat,plus'Nat,timesNat :: Nat -> Nat -> Nat
maxNat      = onValue2 max
minNat      = onValue2 min
timesNat    = onValue2 (*)
plusNat     = onValue2 (+)
-- |`plus'Nat = plusNat` but `encPlus'Nat` ignores carry overflow
plus'Nat    = plusNat
-- |`plusCLANat = plusNat` but `encPlusCLANat` implements a 
-- carry-look-ahead adder
plusCLANat  = plusNat
-- |`plus'CLANat = plusNat` but `encPlus'CLANat` implements a 
-- carry-look-ahead adder that ignores carry overflows
plus'CLANat = plusNat

shiftLNat :: Nat -> Nat
shiftLNat = onValue $ flip B.shiftL 1

shiftRNat :: Nat -> Nat
shiftRNat = onValue $ flip B.shiftR 1

andNat :: Nat -> Nat -> Nat
andNat = onValue2 (.&.)

orNat :: Nat -> Nat -> Nat
orNat = onValue2 (.|.)

xorNat :: Nat -> Nat -> Nat
xorNat = onValue2 B.xor

onValue :: (Integer -> Integer) -> Nat -> Nat
onValue f a = if value a >= 0 
  then nat (width a) $ f $ value a
  else error $ "PreludeNat.onValue: negative value " ++ show a

onValue2 :: (Integer -> Integer -> Integer) -> Nat -> Nat -> Nat
onValue2 f a b =
  if value a >= 0 && value b >= 0
  then nat w $ f (value a) (value b)
  else error $ "PreludeNat.onValue2: negative values " ++ show (a,b)
  where
    w = max (width a) (width b)

onValue2' :: (Integer -> Integer -> a) -> Nat -> Nat -> a
onValue2' f a b = 
  if value a >= 0 && value b >= 0
  then f (value a) (value b)
  else error $ "PreludeNat.onValue2': negative values " ++ show (a,b)

-- * Encoded functions on naturals

encNat,encNatProf :: Int -> Integer -> CO4 EncodedAdt
encNat     0 0 = make (constant True) [] [] False
encNat     w i = make (constant True) (map constant $ toBinary (Just w) i) [] False
encNatProf w i = traced "nat" $ encNat w i

encGtNat,encGeNat,encEqNat,encLeNat,encLtNat,encMaxNat,encMinNat
  ,encGtNatProf,encGeNatProf,encEqNatProf,encLeNatProf,encLtNatProf
  ,encMaxNatProf,encMinNatProf :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encGtNat         = flip encLtNat
encGtNatProf a b = traced "gtNat" $ encGtNat a b

encGeNat         = flip encLeNat
encGeNatProf a b = traced "geNat" $ encGeNat a b

encEqNat = catchInvalid2 $ onFlags2 $ \as bs ->
  zipWithM (\x y -> equals [x,y]) as bs >>= and >>= \r -> return [r]
encEqNatProf a b = traced "eqNat" $ encEqNat a b

encLeNat = catchInvalid2 $ onFlags2 $ \a b -> do
  (l, e) <- encComparePrimitives a b
  r <- or [l,e] 
  return [r]
encLeNatProf a b = traced "leNat" $ encLeNat a b

encLtNat = catchInvalid2 $ onFlags2 $ \a b -> do
  (l, _) <- encComparePrimitives a b
  return [l] 
encLtNatProf a b = traced "ltNat" $ encLtNat a b

encIsZeroNat = catchInvalid $ onFlags $ \a -> do
  nonzero <- or a
  return [ not nonzero ]
encIsZeroNatProf a = traced "isZeroNat" $ encIsZeroNat a

encMaxNat = catchInvalid2 $ onFlags2 $ \ a b -> do
  (l, _) <- encComparePrimitives b a
  zipWithM ( \x y -> ifthenelse l x y ) a b 
encMaxNatProf a b = traced "maxNat" $ encMaxNat a b

encMinNat = catchInvalid2 $ onFlags2 $ \ a b -> do
  (l, _) <- encComparePrimitives a b
  zipWithM ( \x y -> ifthenelse l x y ) a b 
encMinNatProf a b = traced "minNat" $ encMinNat a b

encComparePrimitives :: [Primitive] -> [Primitive] 
                     -> CO4 (Primitive, Primitive) -- ^ (less, equals)
encComparePrimitives = 
    -- TODO: find out which is best for what length
    encComparePrimitives_linear
    -- encComparePrimitives_tree

{-
encComparePrimitives_tree as bs = do
    foldB ( \ (a,b) -> do 
            l <- and [not a, b ]; ne <- xor [ a, b ] ; let e = not ne
            assert [ not l, not e ] -- redundant but possibly helpful? 
            return (l,e) 
          )
          ( \ (l1,e1) (l2,e2) -> do 
            e <- and [e1,e2] 

            -- l1e2 <- and [l1,e2] ; l <- or [l1e2, l2]
            l <- primitive
            implies [ l2 ] [ l ] ; implies [ l1, e2 ] [ l ]
            implies [ not l2, not l1 ] [ not l ]
            implies [ not l2, not e2 ] [ not l ]

            assert [ not l, not e ] -- redundant but possibly helpful?
            return (l,e) 
          )
          ( zip as bs )
-}

implies xs ys = assert (map not xs ++ ys)

foldB u f xs = case xs of
    [ ] -> error "CO4.PreludeNat.foldB _ _ []"
    [x] -> u x
    _   -> do 
        let (pre,post) = splitAt (div (length xs) 2) xs
        a <- foldB u f pre ; b <- foldB u f post
        f a b

encComparePrimitives_linear a b = case (a,b) of
  ([],[]) -> return ( constant False, constant True )
  ((x:xs),(y:ys)) -> do
    ( ll, ee ) <- encComparePrimitives xs ys
    l <- primitive ; e <- primitive

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

    assert [ not l, not e ] -- redundant

    return ( l, e )
{-
  (xs, []) -> do
    x <- or xs
    return (constant False, not x )
  ([], ys) -> do
    y <- or ys
    return ( y, not y )
-}

encPlusNat,encPlusNatProf :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encPlusNat = catchInvalid2 $ onFlags2 $ ripple_carry_adder
encPlusNatProf a b = traced "plusNat" $ encPlusNat a b

encPlus'Nat,encPlus'NatProf :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encPlus'Nat = catchInvalid2 $ onFlags2 $ ripple_carry_adder_with_overflow
encPlus'NatProf a b = traced "plus'Nat" $ encPlus'Nat a b

encPlusCLANat,encPlusCLANatProf :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encPlusCLANat = catchInvalid2 $ onFlags2 $ carry_lookahead_adder
encPlusCLANatProf a b = traced "plusCLANat" $ encPlusCLANat a b

encPlus'CLANat,encPlus'CLANatProf :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encPlus'CLANat = catchInvalid2 $ onFlags2 $ carry_lookahead_adder_with_overflow
encPlus'CLANatProf a b = traced "plus'CLANat" $ encPlus'CLANat a b

-- | semantics: x through c  is  (lin c and x)  or  abs c 
-- Prop False False = delete
-- Prop True False = propagate
-- Prop _ True = create
data Prop = Prop { lin :: Primitive, abs :: Primitive }

multiply :: Prop -> Prop -> CO4 Prop
multiply c1 c2 = do
    l <- and [lin c1, lin c2] ; a <- apply c1 (abs c2)
    return $ Prop { lin = l, abs = a }

apply :: Prop -> Primitive -> CO4 Primitive
apply c p = do d <- and [lin c, p] ; or [abs c, d]

cla as bs = foldB 
            ( \ (a,b) -> do
                (r,c) <- halfAdder a b 
                return ( Prop { lin = r, abs = c } 
                       , \ c -> do x <- xor [a, b, c] ; return [x]
                       ) )
            ( \ (p1, f1) (p2, f2) -> do
                p <- multiply p2 p1
                return ( p, \ ci1 -> do
                          xs <- f1 ci1
                          ci2 <- apply p1 ci1
                          ys <- f2 ci2
                          return $ xs ++ ys
                       ) )
            ( zip as bs )

carry_lookahead_adder as bs = do
    (p, f) <- cla as bs
    assert [ not $ abs p ]
    f (constant False)                    

carry_lookahead_adder_with_overflow as bs = do
    (_, f) <- cla as bs
    -- IGNORE CARRY: assert [ not $ abs p ]
    f (constant False)                    

ripple_carry_adder (a:as) (b:bs) = do
  (z,c) <- halfAdder a b
  zs <- addWithCarry c as bs
  return $ z : zs
  where
    addWithCarry c [] [] = do
      assert [ not c ] 
      return []
    addWithCarry c ( x : xs) ( y:ys ) = do
      (z,d) <- fullAdder c x y
      zs <- addWithCarry d xs ys
      return $ z : zs

ripple_carry_adder_with_overflow (a:as) (b:bs) = do
  (z,c) <- halfAdder a b
  zs <- addWithCarry c as bs
  return $ z : zs
  where
    addWithCarry _ [] [] = return []
    addWithCarry c ( x : xs) ( y:ys ) = do
      (z,d) <- fullAdder c x y
      zs <- addWithCarry d xs ys
      return $ z : zs
{-
addWithCarryW w c xs ys | w <= 0 = do
     forM (c : xs ++ ys) $ \ c -> assert [ not c ]
     return []
addWithCarryW w c ( x : xs) ( y:ys ) = do
          (z,d) <- fullAdder c x y
          zs <- addWithCarryW (w-1) d xs ys
          return $ z : zs
-}

-- formula sizes are similar 
-- but encTimesNat_1 is much (?) better for solver.
-- it is basically a Wallace multiplier
-- https://en.wikipedia.org/wiki/Wallace_tree
-- which has shorter delay than the naive method (encTimesNat_0)

encTimesNat = encTimesNat_1

{-
encTimesNat_0 :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encTimesNat_0 = catchInvalid2 $ onFlags2 $ \ as bs -> do
     let clamp w xs = do
             let (pre, post) = splitAt w xs
             forM post $ \ p -> assert [ not p ]
             return pre
         mul w (a:as) bs = do
             c : cs <- forM bs $ \ b -> and [a,b]
             ds <- clamp (w-1) cs
             case as of
                 [] -> return $ c : ds
                 _ -> do
                     asbs <- mul (w-1) as bs
                     es <- addWithCarryW (w-1) (constant False) ds asbs
                     return $ c : es
     mul (length as) as bs           
  

call o as bs = do
    cs <- forM as $ \ _ -> primitive
    o as bs cs
    return cs
-}

encTimesNat_1 :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encTimesNat_1 = catchInvalid2 $ onFlags2 $ \as bs -> 
    case length as of
        -- 3 -> call Opt.times3 as bs 
        -- 4 -> call Opt.times4 as bs
        -- 5 -> call Opt.times5 as bs
        _ -> wallace_multiplier as bs

wallace_multiplier as bs = do
  kzs <- product_components (Just $ length as) as bs
  export (Just $ length as) kzs

  where
    product_components bound as bs = sequence $ do
        ( i , x ) <- zip [ 0 .. ] as
        ( j , y ) <- zip [ 0 .. ] bs
        return $ do
            z <- and [ x, y ]
            if ( case bound of Nothing -> False ; Just b -> i+j >= b )
                 then do assert [ not z ] ; return []
                 else do return [ ( i+j , [z] ) ]

    export bound kzs = do
        reduce bound $ M.fromListWith (++) $ concat kzs

    reduce bound m = case M.minViewWithKey m of
        Nothing -> return []
        Just ((k, bs), rest ) ->
            if ( case bound of Nothing -> False ; Just b -> k >= b )
            then do
                forM bs $ \ b -> assert [ not b ]
                reduce bound rest
            else let border = Just k == bound in case bs of
                (x:y:z:more) -> do
                    (r,c) <- fullAdder x y z
                    when border $ assert [ not c ]
                    reduce bound $ M.unionWith (++) rest
                           $ M.fromList [ (k, more ++ [r])
                                        , (k+1, [c | Prelude.not border]) ]
                [x,y] ->  do
                    (r,c) <- halfAdder x y 
                    when border $ assert [ not c ]
                    reduce bound $ M.unionWith (++) rest
                           $ M.fromList [ (k, [r])
                                        , (k+1, [c | Prelude.not border]) ]
                [x] -> do
                    xs <- reduce bound rest
                    return $ x : xs

encTimesNatProf a b = traced "timesNat" $ encTimesNat a b

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
    r <- xor [x,y,z]
    c <- primitive
    implies [ x, y ] [ c ]
    implies [ y, z ] [ c ]
    implies [ z, x ] [ c ] 
    implies [ not x, not y ] [ not c ]
    implies [ not y, not z ] [ not c ]
    implies [ not z, not x ] [ not c ]
    return (r,c)

{-
fullAdder_three_with_redundant x y z = do

    r <- xor [x,y,z]

    c <- primitive
    implies [ x, y ] [ c ]
    implies [ y, z ] [ c ]
    implies [ z, x ] [ c ] 
    implies [ not x, not y ] [ not c ]
    implies [ not y, not z ] [ not c ]
    implies [ not z, not x ] [ not c ]

    -- redundant:
    implies [ not r, not c ] [ not x ]
    implies [ not r, not c ] [ not y ]
    implies [ not r, not c ] [ not z ]
    implies [ r, c ] [ x ]
    implies [ r, c ] [ y ]
    implies [ r, c ] [ z ]

    return (r,c)
-}

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

encShiftLNat,encShiftLNatProf :: EncodedAdt -> CO4 EncodedAdt
encShiftLNat = catchInvalid $ onFlags $ \a -> 
  return $ (constant False) : (take (length a - 1) a)
encShiftLNatProf = traced "shiftLNat" . encShiftLNat

encShiftRNat,encShiftRNatProf :: EncodedAdt -> CO4 EncodedAdt
encShiftRNat = catchInvalid $ onFlags $ \a -> 
  return $ tail a ++ [constant False] 
encShiftRNatProf = traced "shiftRNat" . encShiftRNat

encAndNat,encAndNatProf :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encAndNat         = catchInvalid2 $ onFlags2 $ zipWithM $ \x y -> and [x,y]
encAndNatProf a b = traced "andNat" $ encAndNat a b

encOrNat,encOrNatProf :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encOrNat         = catchInvalid2 $ onFlags2 $ zipWithM $ \x y -> or [x,y]
encOrNatProf a b = traced "orNat" $ encOrNat a b

encXorNat,encXorNatProf  :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encXorNat         = catchInvalid2 $ onFlags2 $ zipWithM $ \x y -> xor [x,y]
encXorNatProf a b = traced "xorNat" $ encXorNat a b

onFlags :: ([Primitive] -> CO4 [Primitive]) -> EncodedAdt -> CO4 EncodedAdt
onFlags f a = case flags a of
  Just as -> do flags' <- f as 
                make (definedness a) flags' [] $ isPrefixfree' a
  _       -> abortWithStackTrace "PreludeNat.onFlags: missing flags"

onFlags2 :: ([Primitive] -> [Primitive] -> CO4 [Primitive]) 
         -> EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
onFlags2 f a b = case (flags a, flags b) of
  (Just as, Just bs) -> do
      flags'       <- f as' bs'
      definedness' <- and [definedness a, definedness b]
      make definedness' flags' [] $ prefixfreeBranches [a,b]
    where
      las = length as
      lbs = length bs
      as' = as ++ (replicate (lbs - las) $ constant False)
      bs' = bs ++ (replicate (las - lbs) $ constant False)
  _ -> abortWithStackTrace "PreludeNat.onFlags2: missing flags"

catchInvalid :: (EncodedAdt -> CO4 (EncodedAdt)) -> EncodedAdt -> CO4 (EncodedAdt)
catchInvalid f a = 
  if isConstantlyUndefined a 
  then return encUndefined
  else if isEmpty a 
       then return encEmpty
       else f a

catchInvalid2 :: (EncodedAdt -> EncodedAdt -> CO4 (EncodedAdt)) 
              -> EncodedAdt -> EncodedAdt -> CO4 (EncodedAdt)
catchInvalid2 f a b = 
  if isConstantlyUndefined a || isConstantlyUndefined b
  then return encUndefined
  else if isEmpty a || isEmpty b 
       then return encEmpty
       else f a b
