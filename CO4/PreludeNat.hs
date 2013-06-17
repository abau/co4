{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module CO4.PreludeNat 
  (Nat8, nat8, uNat8, kNat8
  , gtNat8, geNat8, eqNat8, leNat8, ltNat8, maxNat8, minNat8, plusNat8, timesNat8
  , encNat8, encGtNat8, encGeNat8, encEqNat8, encLeNat8, encLtNat8
  , encMaxNat8, encMinNat8, encPlusNat8, encTimesNat8
  )
where

import           Prelude hiding (not,and,or)
import           Control.Monad (liftM,zipWithM,replicateM,forM)
import           Data.Word (Word8)
import qualified Data.Map as M
import           Satchmo.Core.Decode (Decode,decode)
import           Satchmo.Core.SAT.Minisat (SAT)
import           Satchmo.Core.Primitive 
  (Primitive,primitive,constant,assert,not,and,xor,or,equals)
import           Satchmo.Core.MonadSAT (MonadSAT)
import           CO4.EncodedAdt hiding (undefined)
import           CO4.Encodeable (Encodeable (..))
import           CO4.Allocator.Common (Allocator,known,constructors)
import           CO4.Profiling (MonadProfiling,traced)
import           CO4.EncEq (EncEq(..))

profilePreludeNat = False

type Nat8 = Word8

instance (Primitive p, EncodedAdt e p) => Encodeable Nat8 e p where
  encodeConstant i = encodedConstructor (fromIntegral i) (2^8) []

  encode = undefined

instance (Primitive p, EncodedAdt e p, Decode SAT p Bool) => Decode SAT (e p) Nat8 where
  decode p = toIntermediateAdt p (2^8) >>= \case 
    IntermediateUndefined -> error $ "Can not decode 'undefined' to data of type 'Nat8'"
    IntermediateConstructorIndex i _ -> return $ fromIntegral i

instance (Primitive p, EncodedAdt e p) => EncEq Nat8 e p where
  encEq _ a b = tracedWhenProfling "==_Nat8" $ encEqNat8 a b

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

encNat8 :: (Primitive p, EncodedAdt e p, Monad m) => Int -> m (e p)
encNat8 i = return $ encodedConstructor i (2^8) []

encGtNat8,encGeNat8,encEqNat8,encLeNat8,encLtNat8,encMaxNat8,encMinNat8
  :: (Primitive p, EncodedAdt e p, MonadSAT m, MonadProfiling m) => e p -> e p -> m (e p)
encGtNat8 = flip encLtNat8
encGeNat8 = flip encLeNat8
encEqNat8 = onFlags "eqNat8"
  (\as bs -> zipWithM (\x y -> equals [x,y]) as bs >>= and >>= return . make . return)

encLeNat8 = onFlags "leNat8" $ \a b -> do
  (l, e) <- encComparePrimitives a b
  r <- or [l,e] 
  return $ make [r]

encLtNat8 = onFlags "ltNat8" $ \a b -> do
  (l, _) <- encComparePrimitives a b
  return $ make [l]

encMaxNat8 a b = tracedWhenProfling "maxNat8" $ do
  result <- replicateM 8 primitive >>= return . make
  [ra] <- liftM flags' $ encEqNat8 result a
  [rb] <- liftM flags' $ encEqNat8 result b
  [g ] <- liftM flags' $ encGtNat8 a b
  assert [ not g , ra ]
  assert [ g     , rb ]
  return result

encMinNat8 a b = tracedWhenProfling "minNat8" $ do
  result <- replicateM 8 primitive >>= return . make
  [ra] <- liftM flags' $ encEqNat8 result a
  [rb] <- liftM flags' $ encEqNat8 result b
  [l ] <- liftM flags' $ encLtNat8 a b
  assert [ not l , ra ]
  assert [ l     , rb ]
  return result

encComparePrimitives :: (Primitive p, MonadSAT m) 
                     => [p] -> [p] -> m (p, p) -- ^ (less, equals)
encComparePrimitives a b = case (a,b) of
  ([],[]) -> return ( constant False, constant True )
  ((x:xs),(y:ys)) -> do
    l <- and [ not x, y ]
    e <- liftM not $ xor [ x, y ]
    ( ll, ee ) <- encComparePrimitives xs ys
    lee <- and [l,ee]
    l' <- or [ ll, lee ]
    e' <- and [ e, ee ]
    return ( l', e' )
  (xs, []) -> do
    x <- or xs
    return (constant False, not x )
  ([], ys) -> do
    y <- or ys
    return ( y, not y )

encPlusNat8 :: (Primitive p, EncodedAdt e p, MonadSAT m, MonadProfiling m) 
            => e p -> e p -> m (e p)
encPlusNat8 = onFlags "plusNat8" $ \as bs -> do
  zs <- addWithCarry 8 (constant False) as bs
  return $ make zs
  where
    addWithCarry w c xxs yys = case ( xxs, yys ) of
      _ | w <= 0 -> do
          sequence_ $ do p <- c : xxs ++ yys ; return $ assert [ not p ]
          return []
      ( [] , [] ) -> return [ c ]
      ( [], y : ys) -> do
          (r,d) <- halfAdder c y
          rest <- addWithCarry (w-1) d [] ys
          return $ r : rest
      ( _ : _ , [] ) -> addWithCarry w c yys xxs
      ( x : xs, y:ys ) -> do
          (r,d) <- fullAdder c x y
          rest <- addWithCarry (w-1) d xs ys
          return $ r : rest

encTimesNat8 :: (Primitive p, EncodedAdt e p, MonadSAT m, MonadProfiling m) 
            => e p -> e p -> m (e p)
encTimesNat8 = onFlags "timesNat8" $ \as bs -> do
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
            Nothing -> return $ make []
            Just ((k,_) , _) -> do
                  return $ make $ do
                        i <- [ 0 .. k ]
                        let { [ b ] = m M.! i }
                        return b

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
  

fullAdder :: (Primitive p, MonadSAT m) => p -> p -> p -> m (p,p)
fullAdder p1 p2 p3 = do
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

halfAdder :: (Primitive p, MonadSAT m) => p -> p -> m (p,p)
halfAdder p1 p2 = do
  p3 <- primitive
  p4 <- primitive
  assert [p1, not p4]
  assert [p2, not p4]
  assert [not p3, not p4]
  assert [not p1, not p2, not p3]
  assert [not p1, not p2, p4]
  assert [not p1, p2, p3]
  assert [not p1, p3, p4]
  assert [p1, not p2, p3]
  assert [p1, p2, not p3]
  assert [not p2, p3, p4]
  return (p3,p4)

onFlags :: (MonadProfiling m, EncodedAdt e p) 
        => String -> ([p] -> [p] -> m a) -> e p -> e p -> m a
onFlags name f a b = case (flags a, flags b) of
  (Just as, Just bs) -> tracedWhenProfling name $ f as bs
  _                  -> error "PreludeNat.onFlags: missing flags"

tracedWhenProfling :: (MonadProfiling m) => String -> m a -> m a
tracedWhenProfling name action = 
  if profilePreludeNat 
  then traced name action
  else action
