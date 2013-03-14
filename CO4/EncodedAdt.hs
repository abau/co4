{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
module CO4.EncodedAdt
  ( EncodedAdt (EncUndefined), unknown, flags, caseOf, encodedConsCall
  , constructorArgument
  , IntermediateAdt (..), toIntermediateAdt
  )
where

import           Prelude hiding (not,or,and)
import qualified Prelude as P
import           Control.Monad (liftM,forM,forM_,zipWithM)
import qualified Control.Exception as Exception 
import           Data.List (transpose)
import           Data.Maybe (catMaybes,fromMaybe)
import           Data.Tree
import           Satchmo.Core.MonadSAT (MonadSAT)
import           Satchmo.Core.Decode (Decode,decode)
import           Satchmo.Core.Primitive 
  (Primitive,primitive,constant,assert,not,and,implies)
import qualified CO4.AdtIndex as AI
import           CO4.Util (replaceAt,equal,for,toBinary,fromBinary,binaries,bitWidth)

--import Debug.Trace

data EncodedAdt p = EncAdt { flags        :: [p]
                           , constructors :: [EncodedConstructor p]
                           }
                  | EncUndefined

instance Show flag => Show (EncodedAdt flag) where
  show = drawTree . toTree 

data EncodedConstructor p = EncConsNormal [EncodedAdt p] 
                          | EncConsBottom
                          deriving (Show)

isDefined :: EncodedAdt p -> Bool
isDefined = \case EncUndefined -> False
                  _            -> True

isNormal :: EncodedConstructor p -> Bool
isNormal = \case EncConsNormal {} -> True
                 _                -> False

isBottom :: EncodedConstructor p -> Bool
isBottom = \case EncConsBottom -> True
                 _             -> False

unknown :: (MonadSAT m, Primitive p) => AI.AdtIndex -> m (EncodedAdt p)
unknown adtIndex = do
  bits <- sequence $ replicate (AI.staticAdtWidth adtIndex) primitive
  cons <- mapM (encodeConstructor bits) $ AI.constructors adtIndex

  case cons of
    -- cf. single-constructor data types with indirect recursion:
    -- @data Rose a = Node a (List (Rose a))@
    [ EncConsBottom ] -> return EncUndefined 
    _ -> let flags  = bits `AI.atIndex` (AI.flagIndex adtIndex)
             encAdt = EncAdt flags cons
         in do
           excludeBottom encAdt
           excludeInvalidConstructorPatterns encAdt
           return encAdt
  where
    encodeConstructor _     AI.ConsBottom             = return EncConsBottom
    encodeConstructor bits (AI.ConsNormal argIndices) = do
      args <- mapM (encodeArgument bits) argIndices
      if any (P.not . isDefined) args
        then return   EncConsBottom
        else return $ EncConsNormal args

    encodeArgument bits = \case 
      AI.ArgIndex True  index -> encodeStaticArgument bits index
      AI.ArgIndex False index -> unknown index

    encodeStaticArgument bits argIndex = 
      liftM (EncAdt flags') $ mapM (encodeConstructor bits) $ AI.constructors argIndex
      where 
        flags' = bits `AI.atIndex` (AI.flagIndex argIndex)

excludeBottom :: (MonadSAT m, Primitive p) => EncodedAdt p -> m ()
excludeBottom = go 
  where
    go adt = forM_ (zip [0..] (constructors adt)) $ uncurry 
                                                  $ goConstructor 
                                                  $ flags adt

    goConstructor _ _ (EncConsNormal args) = forM_ args go 
    goConstructor flags i EncConsBottom    = 
      let pattern = toBinary (length flags) i
      in
        excludePattern flags pattern

excludeInvalidConstructorPatterns :: (MonadSAT m, Primitive p) => EncodedAdt p -> m ()
excludeInvalidConstructorPatterns = go
  where
    go adt = do
      forM_ nonConstructorPatterns $ excludePattern $ flags adt
      forM_ (constructors adt) goConstructor 

      where
        nonConstructorPatterns = drop (length $ constructors adt)
                               $ binaries $ length $ flags adt

    goConstructor (EncConsNormal args) = forM_ args go
    goConstructor EncConsBottom        = return ()

caseOf :: (MonadSAT m, Primitive p) => EncodedAdt p -> [EncodedAdt p] -> m (EncodedAdt p)
caseOf adt branches = 
  if allBranchesUndefined -- !
  then return EncUndefined
  else do
    flags'        <- caseOfBits (flags adt) $ mapBranches flags
    constructors' <- caseOfConstructors adt $ mapBranches constructors
    return $ EncAdt flags' constructors'
  where
    allBranchesUndefined = all (P.not . isDefined) branches

    mapBranches f = for branches $ \case EncUndefined -> Nothing
                                         branch       -> Just $ f branch

caseOfConstructors :: (MonadSAT m, Primitive p) => EncodedAdt p 
                                                -> [Maybe [EncodedConstructor p]] 
                                                -> m [EncodedConstructor p]
caseOfConstructors adt conss = 
  forM (transpose conss') $ \consT -> 
    if all isBottom consT 
    then return EncConsBottom
    else liftM EncConsNormal 
       $ mapM (caseOf adt) 
       $ transpose 
       $ map (getArgs $ numConsArgs consT) consT
  where 
    conss' = for conss $ \case Just cons -> Exception.assert (length cons == numCons) 
                                            cons
                               Nothing   -> replicate numCons EncConsBottom

    numCons = length $ head $ catMaybes conss

    numConsArgs cons           = length args
      where EncConsNormal args = head $ filter isNormal cons

    getArgs n EncConsBottom        = replicate n EncUndefined
    getArgs n (EncConsNormal args) = Exception.assert (n == length args) args

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
        patterns                = binaries $ length flags 
        selectFlag True  flag   = flag
        selectFlag False flag   = not flag

        mkPremise pattern = and $ zipWith selectFlag pattern flags

    mkBits premises bitsT = zipWithM implies premises bitsT >>= and

excludePattern :: (MonadSAT m, Primitive p) => [p] -> [Bool] -> m ()
excludePattern flags pattern = Exception.assert (length flags == length pattern)
                             $ assert $ zipWith antiSelectBit flags pattern
  where 
    antiSelectBit bit True  = not bit
    antiSelectBit bit False = bit


encodedConsCall :: Primitive p => Int -> Int -> [EncodedAdt p] -> EncodedAdt p
encodedConsCall i numCons args = Exception.assert (i < numCons) $ 
  if containsUndefinedArgs
  then EncUndefined
  else EncAdt flags' constructors'
  where
    flags'                = if numCons > 1 then map constant i' else []
    constructors'         = replaceAt i (EncConsNormal args) 
                          $ replicate numCons EncConsBottom
    i'                    = toBinary (bitWidth numCons) i 
    containsUndefinedArgs = any (P.not . isDefined) args

constructorArgument :: Int -> Int -> EncodedAdt p -> EncodedAdt p
constructorArgument i j = maybe EncUndefined getArg . constructorArguments j
  where 
    getArg args = Exception.assert (i < length args) $ args !! i

constructorArguments :: Int -> EncodedAdt p -> Maybe [EncodedAdt p]
constructorArguments _ EncUndefined  = Nothing
constructorArguments j (EncAdt _ cs) = Exception.assert (j < length cs) $
  case cs !! j of
    EncConsNormal as -> Just as
    EncConsBottom    -> Nothing

-- |The construction of an intermediate ADT simplifies the derivation of the
-- actual @Decode@ instance.
data IntermediateAdt flag = IntermediateConstructorIndex Int [EncodedAdt flag]
                          | IntermediateUndefined
                          deriving Show

toIntermediateAdt :: (MonadSAT m, Primitive p, Decode m p Bool) 
                  => EncodedAdt p -> m (IntermediateAdt p)
toIntermediateAdt adt = case adt of
  EncUndefined                 -> return IntermediateUndefined
  EncAdt {} | null (flags adt) -> return $ intermediate 0
  EncAdt {}                    -> do cons <- liftM fromBinary $ decode $ flags adt 
                                     return $ intermediate cons
  where 
    intermediate i = case constructorArguments i adt of
                        Nothing -> IntermediateUndefined
                        Just as -> IntermediateConstructorIndex i as

toTree :: Show p => EncodedAdt p -> Tree String
toTree adt = case adt of
  EncUndefined    -> Node "undefined" []
  EncAdt fs conss -> Node ("flags: " ++ show fs) $ zipWith consToTree [0..] conss
  where
    consToTree i = \case EncConsNormal args -> Node ("cons " ++ show i) $ map toTree args
                         EncConsBottom      -> Node ("cons " ++ show i ++ ": _|_") []
