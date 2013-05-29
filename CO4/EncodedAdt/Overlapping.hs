{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module CO4.EncodedAdt.Overlapping
  ( EncodedAdt (..), IntermediateAdt (..)
  , bottom, isBottom, flags
  , constantConstructorIndex, caseOf, encodedConstructor, constructorArgument
  , toIntermediateAdt, equalFlags
  )
where

import           Prelude hiding (not,or,and,undefined)
import qualified Prelude as P
import           Control.Monad (forM,zipWithM)
import qualified Control.Exception as Exception 
import           Data.List (transpose)
import           Data.Maybe (catMaybes,fromJust)
import           Data.Tree
import           Satchmo.Core.MonadSAT (MonadSAT)
import           Satchmo.Core.Primitive 
  (Primitive,constant,and,implies,select,evaluateConstant,isConstant,equals)
import           Satchmo.Core.Decode (Decode,decode)
import           CO4.Util (for,toBinary,binaries,bitWidth,fromBinary)

data EncodedAdt p = EncodedAdt [p] [ EncodedAdt p ] 
                  | Bottom
                    deriving (Eq,Ord)

bottom :: EncodedAdt p
bottom = Bottom

isBottom :: EncodedAdt p -> Bool
isBottom Bottom = True
isBottom _      = False

flags :: EncodedAdt p -> Maybe [p]
flags adt | isBottom adt = Nothing
flags (EncodedAdt fs _)  = Just fs

arguments :: EncodedAdt p -> Maybe [ EncodedAdt p ]
arguments adt | isBottom adt = Nothing
arguments (EncodedAdt _ cs)  = Just cs

instance Show flag => Show (EncodedAdt flag) where
  show = drawTree . toTree 
    where
      toTree adt | isBottom adt    = Node "bottom" []
      toTree (EncodedAdt fs conss) = Node ("flags: " ++ " " ++ show fs) $ map toTree conss

constantConstructorIndex :: (Primitive p) => EncodedAdt p -> Maybe Int
constantConstructorIndex adt | isBottom adt   = error "EncodedAdt.constantConstructorIndex: bottom"
constantConstructorIndex (EncodedAdt [] _)    = Just 0
constantConstructorIndex (EncodedAdt flags _) =
  if all isConstant flags
  then Just $ fromBinary $ map (fromJust . evaluateConstant) flags
  else Nothing

caseOf :: (MonadSAT m, Primitive p) => EncodedAdt p -> [EncodedAdt p] 
                                    -> m (EncodedAdt p)
caseOf adt branches | isBottom adt || (all isBottom branches) 
                    = return bottom
caseOf adt branches | length (fromJust $ flags adt) < bitWidth (length branches) 
                    = return bottom --error "EncodedAdt.Overlapping.caseOf: missing flags"
caseOf adt branches =
  case constantConstructorIndex adt of
    Just i  -> Exception.assert (i < length branches) $ return $ branches !! i
    Nothing -> do 
      flags'     <- caseOfBits relevantFlags $ map flags     branches
      arguments' <- caseOfArguments adt'     $ map arguments branches
      return $ EncodedAdt flags' arguments'
  where
    relevantFlags = take (bitWidth $ length branches) $ fromJust $ flags adt
    adt'          = EncodedAdt relevantFlags $ fromJust $ arguments adt

caseOfArguments :: (MonadSAT m, Primitive p) => EncodedAdt p 
                                                -> [Maybe [EncodedAdt p] ]
                                                -> m [EncodedAdt p]
caseOfArguments adt branchArguments = 
  forM (transpose sameSizeBranchArguments) $ caseOf adt
  where 
    sameSizeBranchArguments = for branchArguments $ \case 
      Nothing   -> replicate maxArgs Bottom
      Just args -> args ++ replicate (maxArgs - length args) Bottom

    maxArgs = maximum $ map length $ catMaybes branchArguments

caseOfBits :: (MonadSAT m, Primitive p) => [p] -> [Maybe [p]] -> m [p]
caseOfBits flags branchBits = 
    Exception.assert (P.not $ null nonBottomBits) 
  $ Exception.assert (length flags == bitWidth (length branchBits)) 
  $ do
    premises <- mkPremises
    forM (transpose branchBits') $ mkBits premises
    where
      nonBottomBits  = catMaybes branchBits
      branchBitWidth = maximum $ map length nonBottomBits 
      branchBits'    = for branchBits $ \case
        Nothing -> replicate branchBitWidth $ constant False
        Just bs -> bs ++ replicate (branchBitWidth - (length bs)) (constant False)
      mkPremises     = mapM mkPremise patterns 
        where 
          patterns          = binaries $ length flags 
          mkPremise pattern = and $ zipWith select pattern flags

      mkBits premises bitsT = zipWithM implies premises bitsT >>= and

encodedConstructor :: Primitive p => Int -> Int -> [EncodedAdt p] -> EncodedAdt p
encodedConstructor i n = Exception.assert (i < n) 
                       . EncodedAdt flags 
  where
    flags = case n of 1 -> []
                      _ -> map constant $ toBinary (Just $ bitWidth n) i

constructorArgument :: Primitive p => Int -> Int -> EncodedAdt p -> EncodedAdt p
constructorArgument _ _ adt | isBottom adt  = bottom
constructorArgument i j adt@(EncodedAdt _ args) = 
  case constantConstructorIndex adt of
    Nothing           -> -- Exception.assert (i < length args) $ args !! i
                         if i < length args then args !! i
                                            else Bottom
    Just j' | j == j' -> Exception.assert (i < length args) $ args !! i
    Just _            -> Bottom

data IntermediateAdt p = IntermediateConstructorIndex Int [EncodedAdt p]
                       | IntermediateUndefined

toIntermediateAdt :: (MonadSAT m, Primitive p, Decode m p Bool) 
                  => EncodedAdt p -> Int -> m (IntermediateAdt p)
toIntermediateAdt adt _ | isBottom adt      = return IntermediateUndefined 
toIntermediateAdt (EncodedAdt flags args) n = 
  Exception.assert (length flags >= bitWidth n) $
    if null relevantFlags 
    then return $ intermediate 0
    else decode relevantFlags >>= return . intermediate . fromBinary
    where
      relevantFlags  = take (bitWidth n) flags
      intermediate i = IntermediateConstructorIndex i args 

equalFlags :: (MonadSAT m, Primitive p) => Int -> EncodedAdt p -> EncodedAdt p -> m p
equalFlags n a b = case (flags a, flags b) of
  (Nothing,_) -> return $ constant False -- todo: really?
  (_,Nothing) -> return $ constant False -- todo: really?

  (Just f1, Just f2) -> Exception.assert (b >= length f1 && b >= length f2) $ do
    zipWithM (\x y -> equals [x,y]) (take b f1) (take b f2) >>= and
    where 
      b = bitWidth n
