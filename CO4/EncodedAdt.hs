{-# language FlexibleContexts #-}
{-# language LambdaCase #-}

module CO4.EncodedAdt
  ( EncodedAdt, IntermediateAdt (..)
  , flags, isUnknown, isUndefined, undefined, encode, caseOf, encodedConsCall
  , constructorArgument, toIntermediateAdt
  )
where

import           Prelude hiding (not,or,and,undefined)
import qualified Prelude as P
import           Control.Monad (liftM,forM,forM_,zipWithM)
import qualified Control.Exception as Exception 
import           Data.List (transpose)
import           Data.Maybe (catMaybes,fromMaybe)
import           Data.Tree
import           Satchmo.Core.MonadSAT (MonadSAT)
import           Satchmo.Core.Primitive 
  (Primitive,primitive,constant,assert,and,implies,select,antiSelect)
import           Satchmo.Core.Decode (Decode,decode)
import           CO4.Util (replaceAt,equal,for,toBinary,binaries,bitWidth,fromBinary)
import qualified CO4.Allocator as A

--import Debug.Trace

data EncodedAdt p = KConstructor { constructorIndex :: Int
                                 , numConstructors  :: Int
                                 , arguments        :: [EncodedAdt p]
                                 }
                  | UAdt         { flags        :: [p]
                                 , constructors :: [UnknownConstructor p]
                                 }
                  | Undefined
                  deriving (Eq,Ord)

data UnknownConstructor p = UConstructor [EncodedAdt p] 
                          | UBottom
                          deriving (Eq,Ord)

instance Show flag => Show (EncodedAdt flag) where
  show = drawTree . toTree 

toTree :: Show p => EncodedAdt p -> Tree String
toTree adt = case adt of
  KConstructor {} -> Node (unwords ["constructor", show $ constructorIndex adt
                                   ,"of", show $ numConstructors adt ])
                        $ map toTree $ arguments adt
  Undefined       -> Node "undefined" []
  UAdt fs conss   -> Node ("flags: " ++ show fs) $ zipWith consToTree [0..] conss
  where
    consToTree i = \case UConstructor args -> Node ("cons " ++ show i) $ map toTree args
                         UBottom           -> Node ("cons " ++ show i ++ ": _|_") []

isUnknown,isDefined,isUndefined :: EncodedAdt p -> Bool
isUnknown = \case UAdt {} -> True
                  _       -> False

isDefined = \case Undefined -> False
                  _         -> True

isUndefined = P.not . isDefined

isBottom :: UnknownConstructor p -> Bool
isBottom = \case UBottom -> True
                 _       -> False

undefined :: EncodedAdt p
undefined = Undefined

encode :: (MonadSAT m, Primitive p) => A.Allocator -> m (EncodedAdt p)
encode = \case
  A.Known i n as -> liftM (KConstructor i n) $ mapM encode as
  A.Unknown [allocCons] -> encodeConstructor allocCons >>= \case
    UBottom           -> return Undefined
    UConstructor args -> return $ KConstructor 0 1 args

  A.Unknown allocConss -> do
    flags <- sequence $ replicate (bitWidth $ length allocConss) primitive
    cons  <- mapM encodeConstructor allocConss
    let uAdt = UAdt flags cons
    excludeBottom uAdt
    excludeInvalidConstructorPatterns uAdt
    return uAdt
  where
    encodeConstructor A.AllocateBottom             = return UBottom
    encodeConstructor (A.AllocateConstructor args) = do
      args' <- mapM encode args
      if any (P.not . isDefined) args'
        then return   UBottom
        else return $ UConstructor args'

excludeBottom :: (MonadSAT m, Primitive p) => EncodedAdt p -> m ()
excludeBottom = go 
  where
    go (KConstructor _ _ args) = forM_ args go
    go (UAdt flags conss)      = forM_ (zip [0..] conss) $ uncurry 
                                                         $ goConstructor flags 
    goConstructor _ _     (UConstructor args) = forM_ args go 
    goConstructor flags i  UBottom            = 
      let pattern = toBinary (length flags) i
      in
        excludePattern flags pattern

excludeInvalidConstructorPatterns :: (MonadSAT m, Primitive p) => EncodedAdt p -> m ()
excludeInvalidConstructorPatterns = go
  where
    go (KConstructor _ _ args) = forM_ args go
    go (UAdt flags conss)      = do
      forM_ nonConstructorPatterns $ excludePattern flags 
      forM_ conss goConstructor 

      where
        nonConstructorPatterns = drop (length conss) $ binaries $ length flags 

    goConstructor (UConstructor args) = forM_ args go
    goConstructor  UBottom            = return ()

excludePattern :: (MonadSAT m, Primitive p) => [p] -> [Bool] -> m ()
excludePattern flags pattern = Exception.assert (length flags == length pattern)
                             $ assert $ zipWith antiSelect pattern flags 

caseOf :: (MonadSAT m, Primitive p) => EncodedAdt p -> [EncodedAdt p] -> m (EncodedAdt p)
caseOf adt branches = case adt of
  KConstructor {} -> return $ branches !! (constructorIndex adt)
  Undefined       -> return Undefined
  UAdt {}         ->
    if all isUndefined branches
    then return Undefined
    else do
      flags'        <- caseOfBits (flags adt) $ mapUnknownBranches flags
      constructors' <- caseOfConstructors adt $ mapUnknownBranches constructors
      return $ UAdt flags' constructors'
  where
    mapUnknownBranches f = for unknownBranches $ \case 
                                  Undefined -> Nothing
                                  branch    -> Just $ f branch
    unknownBranches = for branches $ \case
      KConstructor i n args -> toUnknown i n args
      u                     -> u

caseOfConstructors :: (MonadSAT m, Primitive p) => EncodedAdt p 
                                                -> [Maybe [UnknownConstructor p]] 
                                                -> m [UnknownConstructor p]
caseOfConstructors adt conss = 
  forM (transpose conss') $ \consT -> 
    if all isBottom consT 
    then return UBottom
    else liftM UConstructor 
       $ mapM (caseOf adt) 
       $ transpose 
       $ map (getArgs $ numConsArgs consT) consT
  where 
    conss' = for conss $ \case Just cons -> Exception.assert (length cons == numCons) 
                                            cons
                               Nothing   -> replicate numCons UBottom

    numCons = length $ head $ catMaybes conss

    numConsArgs cons = length args
      where UConstructor args = head $ filter (P.not . isBottom) cons

    getArgs n  UBottom            = replicate n Undefined
    getArgs n (UConstructor args) = Exception.assert (n == length args) args

caseOfBits :: (MonadSAT m, Primitive p) => [p] -> [Maybe [p]] -> m [p]
caseOfBits flags bitss = Exception.assert (P.not $ null definedBitss ) $
                         Exception.assert (equal length definedBitss) $ do
  premises <- mkPremises
  forM (transpose bitss') $ mkBits premises
  where
    definedBitss = catMaybes bitss
    bitWidth     = length $ head definedBitss
    bitss'       = map (fromMaybe $ replicate bitWidth $ constant False) bitss
    mkPremises   = mapM mkPremise patterns 
      where 
        patterns          = binaries $ length flags 
        mkPremise pattern = and $ zipWith select pattern flags

    mkBits premises bitsT = zipWithM implies premises bitsT >>= and

toUnknown :: Primitive p => Int -> Int -> [EncodedAdt p] -> EncodedAdt p
toUnknown i numCons args = Exception.assert (i < numCons) $ 
  if containsUndefinedArgs
  then Undefined
  else UAdt flags' constructors'
  where
    flags'                = if numCons > 1 then map constant i' else []
    constructors'         = replaceAt i (UConstructor args) 
                          $ replicate numCons UBottom
    i'                    = toBinary (bitWidth numCons) i 
    containsUndefinedArgs = any (P.not . isDefined) args

encodedConsCall :: Primitive p => Int -> Int -> [EncodedAdt p] -> EncodedAdt p
encodedConsCall i n = Exception.assert (i < n) $ KConstructor i n

constructorArgument :: Int -> Int -> EncodedAdt p -> EncodedAdt p
constructorArgument i j = maybe Undefined getArg . constructorArguments j
  where 
    getArg args = Exception.assert (i < length args) $ args !! i

constructorArguments :: Int -> EncodedAdt p -> Maybe [EncodedAdt p]
constructorArguments j = \case
  KConstructor j' _ args | j == j' -> Just args 
  KConstructor {}                  -> Nothing
  Undefined                        -> Nothing
  UAdt _ conss                     -> Exception.assert (j < length conss) $
    case conss !! j of
      UConstructor args -> Just args
      UBottom           -> Nothing

data IntermediateAdt p = IntermediateConstructorIndex Int [EncodedAdt p]
                       | IntermediateUndefined

toIntermediateAdt :: (MonadSAT m, Primitive p, Decode m p Bool) 
                  => EncodedAdt p -> m (IntermediateAdt p)
toIntermediateAdt adt = case adt of
  KConstructor i _ as   -> return $ IntermediateConstructorIndex i as
  Undefined             -> return IntermediateUndefined 
  UAdt flags _          -> 
    if null flags 
    then return $ intermediate 0
    else decode flags >>= return . intermediate . fromBinary
    where
      intermediate i = case constructorArguments i adt of
        Nothing -> IntermediateUndefined
        Just as -> IntermediateConstructorIndex i as
