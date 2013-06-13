{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module CO4.EncodedAdt
  ( IntermediateAdt (..), EncodedAdt (..)
  , isDefined, isUndefined, isConstantlyDefined, isConstantlyUndefined, isValid
  , isInvalid, caseOfBits)
where

import           Prelude hiding (and,undefined)
import qualified Prelude
import qualified Control.Exception as Exception
import           Control.Monad (forM,zipWithM)
import           Data.List (transpose)
import           Data.Maybe (fromMaybe,catMaybes)
import           Satchmo.Core.Primitive 
  (Primitive,constant,implies,and,select,primitive,assert)
import qualified Satchmo.Core.Primitive as P
import           Satchmo.Core.Decode (Decode)
import           Satchmo.Core.MonadSAT (MonadSAT)
import           CO4.Util (bitWidth,binaries,for)

data IntermediateAdt p = IntermediateConstructorIndex Int [p]
                       | IntermediateUndefined

class Primitive p => EncodedAdt e p where

  undefined :: e p

  isBottom :: e p -> Bool

  flags :: e p -> Maybe [p]

  definedness :: e p -> p

  constantConstructorIndex :: e p -> Maybe Int

  caseOf :: MonadSAT m => e p -> [e p] -> m (e p)

  encodedConstructor :: Int -> Int -> [e p] -> e p

  constructorArgument :: Int -> Int -> e p -> e p

  toIntermediateAdt :: (MonadSAT m, Decode m p Bool) 
                    => e p -> Int -> m (IntermediateAdt (e p))

isDefined,isUndefined :: EncodedAdt e p => e p -> Maybe Bool
isDefined   = P.evaluateConstant . definedness
isUndefined = fmap not . isDefined

isConstantlyDefined,isConstantlyUndefined :: EncodedAdt e p => e p -> Bool
isConstantlyDefined   = fromMaybe False . isDefined
isConstantlyUndefined = fromMaybe False . isUndefined

-- |`isValid x = not ( isConstantlyUndefined x || isBottom x )`
isValid :: EncodedAdt e p => e p -> Bool
isValid x = not ( isConstantlyUndefined x || isBottom x )

isInvalid :: EncodedAdt e p => e p -> Bool
isInvalid = not . isValid

caseOfBits :: (MonadSAT m, Primitive p) => [p] -> [Maybe [p]] -> m [p]
caseOfBits flags branchBits = 
    Exception.assert (not $ null nonBottomBits) 
  $ Exception.assert (length flags == bitWidth (length branchBits)) 
  $ case all equalBits (transpose branchBits') of
      True  -> return $ head $ branchBits'
      False -> forM (transpose branchBits') mergeN 
    where
      nonBottomBits  = catMaybes branchBits
      branchBitWidth = maximum $ map length nonBottomBits 
      branchBits'    = for branchBits $ \case
        Nothing -> replicate branchBitWidth $ constant False
        Just bs -> bs ++ replicate (branchBitWidth - (length bs)) (constant False)

      equalBits bs = all (\b -> b == head bs) bs

      mergeN bitsT = case equalBits bitsT of
        True  -> return $ head bitsT 
        False -> do
           r <- primitive
           forM (zip bitsT (binaries $ length flags)) $ \ (b, pattern) -> do
                let fs = zipWith select pattern flags
                assert ( r : P.not b : map P.not fs  )
                assert ( P.not r :  b : map P.not fs  )
           return r

      caseOf2Bits f as bs = zipWithM merge2 as bs
        where 
          merge2 a b = case a == b of
            True  -> return a
            False -> do
              r <- primitive
              assert [P.not r,       f,       a]
              assert [P.not r, P.not f,       b]
              assert [      r, P.not f, P.not b]
              assert [      r,       f, P.not a]
              return r

