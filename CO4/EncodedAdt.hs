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
import           Data.List (transpose,intercalate)
import           Data.Maybe (catMaybes,fromMaybe)
import           Data.Tree
import           Satchmo.MonadSAT (MonadSAT)
import           Satchmo.Code (Decode,decode)
import           Satchmo.Boolean (Boolean (..),boolean,assert,not,and,implies)
import qualified CO4.AdtIndex as AI
import           CO4.Util (replaceAt,equal,for,toBinary,fromBinary,binaries,bitWidth)

--import Debug.Trace

data EncodedAdt = EncAdt { flags        :: [Boolean]
                         , constructors :: [EncodedConstructor]
                         }
                | EncUndefined

data EncodedConstructor = EncConsNormal [EncodedAdt]
                        | EncConsBottom
                        deriving (Show)

isDefined :: EncodedAdt -> Bool
isDefined = \case EncUndefined -> False
                  _            -> True

isNormal :: EncodedConstructor -> Bool
isNormal = \case EncConsNormal {} -> True
                 _                -> False

isBottom :: EncodedConstructor -> Bool
isBottom = \case EncConsBottom -> True
                 _             -> False

instance Show EncodedAdt where
  show = drawTree . toTree 

showBoolean :: Boolean -> String
showBoolean (Constant c) = show c
showBoolean (Boolean  b) = show b

showBooleans :: [Boolean] -> String
showBooleans bs = "[" ++ (intercalate ", " $ map showBoolean bs) ++ "]"

unknown :: MonadSAT m => AI.AdtIndex -> m EncodedAdt
unknown adtIndex = do
  bits <- sequence $ replicate (AI.staticAdtWidth adtIndex) boolean
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

excludeBottom :: MonadSAT m => EncodedAdt -> m ()
excludeBottom = go []
  where
    go :: MonadSAT m => [([Boolean],Int)] -> EncodedAdt -> m ()
    go prefix adt = forM_ (zip (constructors adt) [0..]) $ \(cons,i) ->
      let prefix' = prefix ++ [(flags adt,i)]
      in
        goConstructor prefix' cons

    goConstructor prefix (EncConsNormal args) = forM_ args $ go prefix
    goConstructor prefix EncConsBottom        = uncurry excludePattern 
                                              $ foldl makePattern ([],[]) prefix
      where 
        makePattern (flags',bits') ([],0) = (flags',bits')

        makePattern (flags',bits') (flags,consIndex) =
          (flags' ++ flags, bits' ++ (toBinary (length flags) consIndex))

excludeInvalidConstructorPatterns :: MonadSAT m => EncodedAdt -> m ()
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

caseOf :: MonadSAT m => EncodedAdt -> [EncodedAdt] -> m EncodedAdt
caseOf adt branches = 
  if allBranchesUndefined -- !
  then return EncUndefined
  else do
    flags'        <- caseOfBits (flags adt) $ mapBranches flags
    constructors' <- caseOfConstructors adt $ catMaybes $ mapBranches constructors
    return $ EncAdt flags' constructors'
  where
    allBranchesUndefined = all (P.not . isDefined) branches

    mapBranches f = for branches $ \case EncUndefined -> Nothing
                                         branch       -> Just $ f branch

caseOfConstructors :: MonadSAT m => EncodedAdt -> [[EncodedConstructor]] 
                   -> m [EncodedConstructor]
caseOfConstructors adt conss = Exception.assert (equal length conss) $ 
  forM (transpose conss) $ \consT -> 
    if all isBottom consT 
    then return EncConsBottom
    else liftM EncConsNormal 
       $ mapM (caseOf adt) 
       $ transpose 
       $ map (getArgs $ numConsArgs consT) consT
  where 
    numConsArgs cons = length args
      where EncConsNormal args = head $ filter isNormal cons

    getArgs n EncConsBottom        = replicate n EncUndefined
    getArgs n (EncConsNormal args) = Exception.assert (n == length args) args

caseOfBits :: MonadSAT m => [Boolean] -> [Maybe [Boolean]] -> m [Boolean]
caseOfBits flags bitss = Exception.assert (P.not $ null definedBitss ) $
                         Exception.assert (equal length definedBitss) $ do
  premises <- mkPremises
  forM (transpose bitss') $ mkBits premises
  where
    definedBitss = catMaybes bitss
    bitWidth     = length $ head definedBitss
    bitss'       = map (fromMaybe $ replicate bitWidth $ Constant False) bitss
    mkPremises   = mapM mkPremise patterns 
      where 
        patterns                = binaries $ length flags 
        selectFlag True  flag   = flag
        selectFlag False flag   = not flag

        mkPremise pattern = and $ zipWith selectFlag pattern flags

    mkBits premises bitsT = zipWithM implies premises bitsT >>= and

excludePattern :: MonadSAT m => [Boolean] -> [Bool] -> m ()
excludePattern flags pattern = Exception.assert (length flags == length pattern)
                             $ assert $ zipWith antiSelectBit flags pattern
  where 
    antiSelectBit bit True  = not bit
    antiSelectBit bit False = bit


encodedConsCall :: Int -> Int -> [EncodedAdt] -> EncodedAdt
encodedConsCall i numCons args = Exception.assert (i < numCons) $ 
  if containsUndefinedArgs
  then EncUndefined
  else EncAdt flags' constructors'
  where
    flags'                = if numCons > 1 then map Constant i' else []
    constructors'         = replaceAt i (EncConsNormal args) 
                          $ replicate numCons EncConsBottom
    i'                    = toBinary (bitWidth numCons) i 
    containsUndefinedArgs = any (P.not . isDefined) args

constructorArgument :: Int -> Int -> EncodedAdt -> EncodedAdt
constructorArgument i j = maybe EncUndefined getArg . constructorArguments j
  where 
    getArg args = Exception.assert (i < length args) $ args !! i

constructorArguments :: Int -> EncodedAdt -> Maybe [EncodedAdt]
constructorArguments j adt = Exception.assert (j < length (constructors adt)) $
  case adt of
    EncUndefined -> Nothing
    EncAdt _ cs  -> case cs !! j of
      EncConsNormal as -> Just as
      EncConsBottom    -> Nothing

-- |The construction of an intermediate ADT simplifies the derivation of the
-- actual @Decode@ instance.
data IntermediateAdt = IntermediateConstructorIndex Int [EncodedAdt]
                     | IntermediateUndefined
                     deriving Show

toIntermediateAdt :: (MonadSAT m, Decode m Boolean Bool) 
                  => EncodedAdt -> m IntermediateAdt 
toIntermediateAdt adt = case adt of
  EncUndefined                 -> return IntermediateUndefined
  EncAdt {} | null (flags adt) -> return $ intermediate 0
  EncAdt {}                    -> do cons <- liftM fromBinary $ decode $ flags adt 
                                     return $ intermediate cons
  where 
    intermediate :: Int -> IntermediateAdt
    intermediate i = case constructorArguments i adt of
                        Nothing -> IntermediateUndefined
                        Just as -> IntermediateConstructorIndex i as

toTree :: EncodedAdt -> Tree String
toTree adt = case adt of
  EncUndefined    -> Node "undefined" []
  EncAdt fs conss -> Node ("flags: " ++ showBooleans fs) $ zipWith consToTree [0..] conss
  where
    consToTree i = \case EncConsNormal args -> Node ("cons " ++ show i) $ map toTree args
                         EncConsBottom      -> Node ("cons " ++ show i ++ ": _|_") []
