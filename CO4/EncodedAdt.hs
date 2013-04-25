{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module CO4.EncodedAdt
  ( EncodedAdt (..), EncodedConstructor, IntermediateAdt (..)
  , bottom, isBottom, flags, bottomConstructor, isBottomConstructor
  , constantConstructorIndex, caseOf, encodedConstructor, constructorArgument
  , toIntermediateAdt
  )
where

import           Prelude hiding (not,or,and,undefined)
import qualified Prelude as P
import           Control.Monad (forM,zipWithM)
import qualified Control.Exception as Exception 
import           Data.List (transpose)
import           Data.Maybe (catMaybes,fromMaybe,fromJust)
import           Data.Tree
import           Satchmo.Core.MonadSAT (MonadSAT)
import           Satchmo.Core.Primitive 
  (Primitive,constant,and,implies,select,evaluateConstant,isConstant)
import           Satchmo.Core.Decode (Decode,decode)
import           CO4.Util (replaceAt,equal,for,toBinary,binaries,bitWidth,fromBinary)

data EncodedAdt p = EncodedAdt [p] [ EncodedConstructor p ] deriving (Eq,Ord)

type EncodedConstructor p = [EncodedAdt p]

bottom :: EncodedAdt p
bottom = EncodedAdt [] []

isBottom :: EncodedAdt p -> Bool
isBottom (EncodedAdt [] []) = True
isBottom (EncodedAdt _  []) = error "EncodedAdt.isBottom: orphaned flags"
isBottom (EncodedAdt _ _)   = False

flags :: EncodedAdt p -> Maybe [p]
flags adt | isBottom adt = Nothing
flags (EncodedAdt fs _)  = Just fs

constructors :: EncodedAdt p -> Maybe [ EncodedConstructor p ]
constructors adt | isBottom adt = Nothing
constructors (EncodedAdt _ cs)  = Just cs

constructor :: Int -> EncodedAdt p -> Maybe (EncodedConstructor p)
constructor j adt = constructors adt >>= getConstructor
  where
    getConstructor conss = if isBottomConstructor c then Nothing
                                                    else Just c
      where c = conss !! j

bottomConstructor :: EncodedConstructor p
bottomConstructor = [ bottom ]

isBottomConstructor :: EncodedConstructor p -> Bool
isBottomConstructor [b] = isBottom b
isBottomConstructor _   = False

instance Show flag => Show (EncodedAdt flag) where
  show = drawTree . toTree 
    where
      toTree adt | isBottom adt    = Node "bottom" []
      toTree (EncodedAdt fs conss) = Node ("flags: " ++ show fs) 
                                   $ zipWith consToTree [0..] conss
      consToTree i = Node ("cons " ++ show i) . map toTree 

constantConstructorIndex :: (Primitive p) => EncodedAdt p -> Maybe Int
constantConstructorIndex adt | isBottom adt   = error "EncodedAdt.constantConstructorIndex: bottom"
constantConstructorIndex (EncodedAdt flags _) =
  if all isConstant flags
  then Just $ fromBinary $ map (fromJust . evaluateConstant) flags
  else Nothing

caseOf :: (MonadSAT m, Primitive p) => EncodedAdt p -> [EncodedAdt p] 
                                    -> m (EncodedAdt p)
caseOf adt branches | isBottom adt || (all isBottom branches) = return bottom
caseOf adt branches = case constantConstructorIndex adt of
  Just i  -> return $ branches !! i
  Nothing -> do
    flags'        <- caseOfBits (fromJust $ flags adt) $ map flags        branches
    constructors' <- caseOfConstructors adt            $ map constructors branches
    return $ EncodedAdt flags' constructors'

caseOfConstructors :: (MonadSAT m, Primitive p) => EncodedAdt p 
                                                -> [Maybe [EncodedConstructor p] ]
                                                -> m [EncodedConstructor p]
caseOfConstructors adt branchConstructors = 
  forM (transpose sameSizeBranchConstructors) $ \consT -> 
    if all isBottomConstructor consT 
    then return bottomConstructor
    else mapM (caseOf adt) 
       $ transpose 
       $ map (getArgs $ numConsArgs consT) consT
  where 
    sameSizeBranchConstructors = for branchConstructors $ \case 
      Nothing   -> replicate numCons bottomConstructor
      Just cons -> cons

    numCons = length $ head $ catMaybes branchConstructors

    numConsArgs = length . head . filter (P.not . isBottomConstructor)

    getArgs n c | isBottomConstructor c = replicate n bottom
    getArgs n args                      = Exception.assert (n == length args) args

caseOfBits :: (MonadSAT m, Primitive p) => [p] -> [Maybe [p]] -> m [p]
caseOfBits flags branchBits = Exception.assert (P.not $ null nonBottomBits) $
                              Exception.assert (equal length nonBottomBits) $ do
  premises <- mkPremises
  forM (transpose branchBits') $ mkBits premises
  where
    nonBottomBits = catMaybes branchBits
    bitWidth      = length $ head nonBottomBits
    branchBits'   = map (fromMaybe $ replicate bitWidth $ constant False) branchBits
    mkPremises    = mapM mkPremise patterns 
      where 
        patterns          = binaries $ length flags 
        mkPremise pattern = and $ zipWith select pattern flags

    mkBits premises bitsT = zipWithM implies premises bitsT >>= and

encodedConstructor :: Primitive p => Int -> Int -> [EncodedAdt p] -> EncodedAdt p
encodedConstructor i n args = Exception.assert (i < n) 
                            $ EncodedAdt flags constructors
  where
    flags        = map constant $ toBinary (bitWidth n) i
    constructors = replaceAt i args
                 $ replicate n bottomConstructor

constructorArgument :: Int -> Int -> EncodedAdt p -> EncodedAdt p
constructorArgument _ _ adt | isBottom adt = bottom
constructorArgument i j adt = maybe bottom getArg $ constructor j adt
  where 
    getArg args = Exception.assert (i < length args) $ args !! i

data IntermediateAdt p = IntermediateConstructorIndex Int [EncodedAdt p]
                       | IntermediateUndefined

toIntermediateAdt :: (MonadSAT m, Primitive p, Decode m p Bool) 
                  => EncodedAdt p -> m (IntermediateAdt p)
toIntermediateAdt adt | isBottom adt       = return IntermediateUndefined 
toIntermediateAdt adt@(EncodedAdt flags _) =
  if null flags 
  then return $ intermediate 0
  else decode flags >>= return . intermediate . fromBinary
  where
    intermediate i = case constructor i adt of
      Nothing -> IntermediateUndefined
      Just as -> IntermediateConstructorIndex i as
