{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module CO4.Prelude.Nat 
  ( Nat, width, value, nat, uNat, knownNat
  , gtNat, geNat, eqNat, leNat, ltNat
  , isZeroNat
  , maxNat, minNat, timesNat
  , plusNat, plus'Nat, plusCLANat, plus'CLANat
  , shiftLNat, shiftRNat, andNat, orNat, xorNat

  , encNat, encGtNat, encGeNat, encEqNat, encLeNat, encLtNat
  , encIsZeroNat
  , encMaxNat, encMinNat, encTimesNat
  , encPlusNat, encPlus'Nat
  , encShiftLNat, encShiftRNat, encAndNat, encOrNat, encXorNat

  , encNatProf
  , encGtNatProf, encGeNatProf, encEqNatProf, encLeNatProf, encLtNatProf
  , encIsZeroNatProf
  , encMaxNatProf, encMinNatProf, encTimesNatProf
  , encPlusNatProf, encPlus'NatProf
  , encShiftLNatProf, encShiftRNatProf, encAndNatProf, encOrNatProf, encXorNatProf

  , onUnwrappedFlags1, onUnwrappedFlags2
  )
where

import           Prelude hiding (not,and,or,abs)
import qualified Prelude
import           Control.Monad (zipWithM,forM, when)
import           Control.Monad.Writer (WriterT,lift,runWriterT,tell)
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
import           CO4.Algorithms.UndefinedValues.Data (onUnwrapped1,onUnwrapped2)

--import qualified CO4.PreludeNat.Opt as Opt

data Nat = Nat { width :: Int
               , value :: Integer
               }

instance Eq Nat where
  (==) = eqNat

instance Ord Nat where
  compare = compare `on` value

instance Show Nat where
  show = show . value

instance Encodeable Nat where
  encode = encNat . value

instance Decode SAT EncodedAdt Nat where
  decode p = case flags p of
    Just [] -> return $ Nat 0 0
    Just fs -> decode fs >>= (return . (Nat $ length fs) . fromBinary)
    Nothing -> error "Missing flags while decoding 'Nat'"

instance FromKnown Nat where
  fromKnown = knownNat . value

instance Complete Nat where
  complete = uNat maxBound

uNat :: Int -> TAllocator Nat
uNat = unsafeTAllocator . builtInUnknown

knownNat :: Integer -> TAllocator Nat
knownNat = unsafeTAllocator . BuiltInKnown . toBinary Nothing

-- * Plain functions on naturals

nat :: Integer -> Nat
nat n = Nat (bitWidth $ n + 1) n

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
  then nat $ f $ value a
  else error $ "PreludeNat.onValue: negative value " ++ show a

onValue2 :: (Integer -> Integer -> Integer) -> Nat -> Nat -> Nat
onValue2 f a b =
  if value a >= 0 && value b >= 0
  then nat $ f (value a) (value b)
  else error $ "PreludeNat.onValue2: negative values " ++ show (a,b)

onValue2' :: (Integer -> Integer -> a) -> Nat -> Nat -> a
onValue2' f a b = 
  if value a >= 0 && value b >= 0
  then f (value a) (value b)
  else error $ "PreludeNat.onValue2': negative values " ++ show (a,b)

-- * Encoded functions on naturals

encNat,encNatProf :: Integer -> CO4 EncodedAdt
encNat     i = make (map constant $ toBinary Nothing i) [] False
encNatProf i = traced "nat" $ encNat i

encGtNat,encGeNat,encEqNat,encLeNat,encLtNat,encMaxNat,encMinNat
  ,encGtNatProf,encGeNatProf,encEqNatProf,encLeNatProf,encLtNatProf
  ,encMaxNatProf,encMinNatProf :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encGtNat         = flip encLtNat
encGtNatProf a b = traced "gtNat" $ encGtNat a b

encGeNat         = flip encLeNat
encGeNatProf a b = traced "geNat" $ encGeNat a b

encEqNat = onUnwrappedFlags2 $ \as bs -> do
  result <- zipWithM (\x y -> equals [x,y]) as bs >>= and 
  return (constant True, [result])
encEqNatProf a b = traced "eqNat" $ encEqNat a b

encLeNat = onUnwrappedFlags2 $ \a b -> do
  (l, e) <- encComparePrimitives a b
  result <- or [l,e] 
  return (constant True, [result])
encLeNatProf a b = traced "leNat" $ encLeNat a b

encLtNat = onUnwrappedFlags2 $ \a b -> do
  (l, _) <- encComparePrimitives a b
  return (constant True, [l])
encLtNatProf a b = traced "ltNat" $ encLtNat a b

encIsZeroNat = onUnwrappedFlags1 $ \a -> do
  nonzero <- or a
  return (constant True, [ not nonzero ])
encIsZeroNatProf a = traced "isZeroNat" $ encIsZeroNat a

encMaxNat = onUnwrappedFlags2 $ \ a b -> do
  (l, _) <- encComparePrimitives b a
  result <- zipWithM ( \x y -> ifthenelse l x y ) a b 
  return (constant True, result)
encMaxNatProf a b = traced "maxNat" $ encMaxNat a b

encMinNat = onUnwrappedFlags2 $ \ a b -> do
  (l, _) <- encComparePrimitives a b
  result <- zipWithM ( \x y -> ifthenelse l x y ) a b 
  return (constant True, result)
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

foldB u f xs = case xs of
    [ ] -> error "CO4.PreludeNat.foldB _ _ []"
    [x] -> u x
    _   -> do 
        let (pre,post) = splitAt (div (length xs) 2) xs
        a <- foldB u f pre ; b <- foldB u f post
        f a b
-}

implies xs ys = assert (map not xs ++ ys)

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
encPlusNat = onUnwrappedFlags2 $ \a b -> do
  (carry, result) <- ripple_carry_adder a b
  return (not carry, result)
encPlusNatProf a b = traced "plusNat" $ encPlusNat a b

encPlus'Nat,encPlus'NatProf :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encPlus'Nat = onUnwrappedFlags2 $ \a b -> do
  result <- ripple_carry_adder_with_overflow a b
  return (constant True, result)
encPlus'NatProf a b = traced "plus'Nat" $ encPlus'Nat a b

ripple_carry_adder :: [Primitive] -> [Primitive] -> CO4 (Primitive, [Primitive])
ripple_carry_adder (a:as) (b:bs) = do
  (z,c)   <- halfAdder a b
  (c',zs) <- addWithCarry c as bs
  return (c', z : zs)
  where
    addWithCarry c [] [] = do
      return (c, [])
    addWithCarry c ( x : xs) ( y:ys ) = do
      (z,c')   <- fullAdder c x y
      (c'',zs) <- addWithCarry c' xs ys
      return (c'', z : zs)

ripple_carry_adder_with_overflow :: [Primitive] -> [Primitive] -> CO4 [Primitive]
ripple_carry_adder_with_overflow a b = ripple_carry_adder a b >>= return . snd
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
encTimesNat_0 = onUnwrappedFlags2 $ \ as bs -> do
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
encTimesNat_1 = onUnwrappedFlags2 $ \as bs -> 
  let f = case length as of
            -- 3 -> call Opt.times3
            -- 4 -> call Opt.times4
            -- 5 -> call Opt.times5
            _ -> wallace_multiplier
  in do
    (carry, result) <- f as bs
    return (not carry, result)

wallace_multiplier :: [Primitive] -> [Primitive] -> CO4 (Primitive, [Primitive])
wallace_multiplier as bs = do
  (result, carries) <- runWriterT $ do
    kzs <- product_components (Just $ length as) as bs
    export (Just $ length as) kzs

  carry <- or carries
  return (carry, result)

  where
    carry c = tell [c]

    product_components bound as bs = sequence $ do
        ( i , x ) <- zip [ 0 .. ] as
        ( j , y ) <- zip [ 0 .. ] bs
        return $ do
            z <- and [ x, y ]
            if ( case bound of Nothing -> False ; Just b -> i+j >= b )
                 then do carry z ; return []
                 else do return [ ( i+j , [z] ) ]

    export bound kzs = do
        reduce bound $ M.fromListWith (++) $ concat kzs

    reduce :: Maybe Int -> M.Map Int [Primitive] -> WriterT [Primitive] CO4 [Primitive]
    reduce bound m = case M.minViewWithKey m of
        Nothing -> return []
        Just ((k, bs), rest ) ->
            if ( case bound of Nothing -> False ; Just b -> k >= b )
            then do
                forM bs $ \ b -> carry b
                reduce bound rest
            else let border = Just k == bound in case bs of
                (x:y:z:more) -> do
                    (r,c) <- lift $ fullAdder x y z
                    when border $ carry c
                    reduce bound $ M.unionWith (++) rest
                           $ M.fromList [ (k, more ++ [r])
                                        , (k+1, [c | Prelude.not border]) ]
                [x,y] ->  do
                    (r,c) <- lift $ halfAdder x y 
                    when border $ carry c
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
encShiftLNat = onUnwrappedFlags1 $ \a -> 
  return (constant True, (constant False) : (take (length a - 1) a))
encShiftLNatProf = traced "shiftLNat" . encShiftLNat

encShiftRNat,encShiftRNatProf :: EncodedAdt -> CO4 EncodedAdt
encShiftRNat = onUnwrappedFlags1 $ \a -> 
  return (constant True, tail a ++ [constant False])
encShiftRNatProf = traced "shiftRNat" . encShiftRNat

encAndNat,encAndNatProf :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encAndNat         = onUnwrappedFlags2 $ \a b -> do
  result <- zipWithM (\x y -> and [x,y]) a b
  return (constant True, result)
encAndNatProf a b = traced "andNat" $ encAndNat a b

encOrNat,encOrNatProf :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encOrNat         = onUnwrappedFlags2 $ \a b -> do
  result <- zipWithM (\x y -> or [x,y]) a b
  return (constant True, result)
encOrNatProf a b = traced "orNat" $ encOrNat a b

encXorNat,encXorNatProf  :: EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
encXorNat         = onUnwrappedFlags2 $ \a b -> do
  result <- zipWithM (\x y -> xor [x,y]) a b
  return (constant True, result)
encXorNatProf a b = traced "xorNat" $ encXorNat a b

onUnwrappedFlags1 :: ([Primitive] -> CO4 (Primitive,[Primitive])) 
                  -> EncodedAdt -> CO4 EncodedAdt
onUnwrappedFlags1 f = onUnwrapped1 onFlags
  where
    onFlags a = if isEmpty a 
      then return (constant True, encEmpty)
      else case flags a of
        Just as -> do (def,flags') <- f as 
                      result       <- make flags' [] $ isPrefixfree' a
                      return (def, result)
        _       -> abortWithStackTrace "PreludeNat.onUnwrappedFlags: missing flags"

onUnwrappedFlags2 :: ([Primitive] -> [Primitive] -> CO4 (Primitive,[Primitive]))
                  -> EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
onUnwrappedFlags2 f = onUnwrapped2 onFlags
  where
    onFlags a b = if isEmpty a || isEmpty b
      then return (constant True, encEmpty)
      else case (flags a, flags b) of
        (Just as, Just bs) -> do
            (def,flags') <- f as' bs'
            result       <- make flags' [] $ allBranchesPrefixfree [a,b]
            return (def,result)
          where
            las = length as
            lbs = length bs
            as' = as ++ (replicate (lbs - las) $ constant False)
            bs' = bs ++ (replicate (las - lbs) $ constant False)
        _ -> abortWithStackTrace "PreludeNat.onUnwrappedFlags2: missing flags"
