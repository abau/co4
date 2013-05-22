{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module CO4.EncodedAdt.Overlapping2
  ( EncodedAdt (..), IntermediateAdt (..)
  , isBottom, flags
  , constantConstructorIndex, caseOf, encodedConstructor, constructorArgument
  , toIntermediateAdt
  )
where

import           Control.Exception (assert)
import           Data.List (intercalate)
import           Data.Maybe (fromJust,catMaybes)
import           Satchmo.Core.MonadSAT (MonadSAT)
import           Satchmo.Core.Primitive (Primitive)
import           Satchmo.Core.Decode (Decode,decode)
import           CO4.Util (replaceAt,bitWidth,fromBinary)
import           CO4.Allocator.Common (Allocator (..),AllocateConstructor (..))
import qualified CO4.EncodedAdt.Overlapping as O

data EncodedAdt p = EncodedAdt (Maybe Allocator) (O.EncodedAdt p)
                    deriving (Eq,Ord,Show)

allocator (EncodedAdt a _) = a
oEncodedAdt (EncodedAdt _ o)  = o

liftO :: (O.EncodedAdt p -> a) -> EncodedAdt p -> a
liftO f (EncodedAdt _ e) = f e

liftOM :: Monad m => (O.EncodedAdt p -> m a) -> EncodedAdt p -> m a
liftOM f (EncodedAdt _ e) = f e

isBottom :: EncodedAdt p -> Bool
isBottom = liftO O.isBottom

flags :: Primitive p => EncodedAdt p -> Maybe [p]
flags = liftO O.flags -- . restrict

constantConstructorIndex :: (Primitive p) => EncodedAdt p -> Maybe Int
constantConstructorIndex = liftO O.constantConstructorIndex -- . restrict

caseOf :: (MonadSAT m, Primitive p) => EncodedAdt p -> [EncodedAdt p] 
                                    -> m (EncodedAdt p)
caseOf adt branches = {- traceShow ("--",branches) $-} do
  result <- O.caseOf (oEncodedAdt $ restrictBy (length branches) adt) 
          $ map (oEncodedAdt {-. restrict-}) branches
  {-
  result <- O.caseOf (oEncodedAdt {-$ restrict-} adt) 
          $ map (oEncodedAdt {-. restrict-}) branches
          -}
  return $ -- restrict $
    if O.isBottom result
    then EncodedAdt Nothing result
    else EncodedAdt (mergeAllocs $ catMaybes $ map allocator branches) result

  where
    mergeAllocs [] = Nothing
    mergeAllocs as = Just $ foldl1 merge as

    merge a@(Known i1 n1 args1) b@(Known i2 n2 args2) =
        assert (n1 == n2)
      $ if i1 == i2 
        then  assert (length args1 == length args2) 
            $ Known i1 n1 $ zipWith merge args1 args2

        else Unknown $ replaceAt i1 (AllocateConstructor args1)
                     $ replaceAt i2 (AllocateConstructor args2)
                     $ replicate n1 AllocateBottom

    merge (Known i n args) (Unknown cons) = 
        assert (n == length cons)
      $ case cons !! i of
          AllocateBottom -> 
            Unknown $ replaceAt i (AllocateConstructor args)
                    $ cons 

          AllocateConstructor args' -> assert (length args == length args') $ 
            Unknown $ replaceAt i (AllocateConstructor $ zipWith merge args args')
                    $ cons 

    merge a@(Unknown {}) b@(Known {}) = merge b a

    merge (Unknown cons1) (Unknown cons2) = assert (length cons1 == length cons2) $
      Unknown $ zipWith mergeCons cons1 cons2

    mergeCons AllocateBottom a = a
    mergeCons a AllocateBottom = a
    mergeCons (AllocateConstructor args1) (AllocateConstructor args2) =
      assert (length args1 == length args2) $ AllocateConstructor 
                                            $ zipWith merge args1 args2

encodedConstructor :: Primitive p => Int -> Int -> [EncodedAdt p] -> EncodedAdt p
encodedConstructor i n args = 
  if any (O.isBottom . oEncodedAdt) args
  then EncodedAdt Nothing O.bottom
  else EncodedAdt (Just $ Known i n $ map (fromJust . allocator) args)
           $ O.encodedConstructor i n 
           $ map oEncodedAdt args

constructorArgument :: Primitive p => Int -> Int -> EncodedAdt p -> EncodedAdt p
constructorArgument i j e = 
  let e' = liftO (O.constructorArgument i j) e
  in
    {-restrict $-} case allocator e of
      Nothing -> assert (O.isBottom e') $ EncodedAdt Nothing e'
      Just (Known j' n' args) | j == j' -> 
        EncodedAdt (Just $ args !! i) e'

      Just (Known {}) -> assert (O.isBottom e') $ EncodedAdt Nothing e'

      Just (Unknown cons) -> 
        case cons !! j of
          --AllocateBottom           -> assert (O.isBottom e') $ EncodedAdt Nothing e'
          AllocateBottom           -> EncodedAdt Nothing O.bottom
          AllocateConstructor args -> EncodedAdt (Just $ args !! i) e'

restrictBy :: Int -> EncodedAdt p -> EncodedAdt p
restrictBy n (EncodedAdt alloc (O.EncodedAdt flags args)) = 
  assert (length flags >= bitWidth n) $
    EncodedAdt alloc $ O.EncodedAdt (take (bitWidth n) flags) args

data IntermediateAdt p = IntermediateConstructorIndex Int [EncodedAdt p]
                       | IntermediateUndefined

toIntermediateAdt :: (MonadSAT m, Primitive p, Decode m p Bool) 
                  => EncodedAdt p -> Int -> m (IntermediateAdt p)
toIntermediateAdt adt _ | O.isBottom (oEncodedAdt adt) = return IntermediateUndefined 
toIntermediateAdt (EncodedAdt _ (O.EncodedAdt flags args)) n = 
  assert (length flags >= bitWidth n) $
    if null relevantFlags 
    then return $ intermediate 0
    else decode relevantFlags >>= return . intermediate . fromBinary
    where
      relevantFlags  = take (bitWidth n) flags
      intermediate i = IntermediateConstructorIndex i (map (EncodedAdt undefined) args)
