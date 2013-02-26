{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
module CO4.EncodedAdt
  ( EncodedAdt (EncUndefined), unknown, flags, switchBy, encodedConsCall
  , constructorArgument
  , IntermediateAdt (..), toIntermediateAdt
  )
where

import           Prelude hiding (not,or,and)
import qualified Prelude as P
import           Control.Monad (ap,forM,zipWithM)
import qualified Control.Exception as Exception 
import           Data.List (transpose,intercalate)
import           Data.Maybe (catMaybes)
import           Satchmo.Code 
import           Satchmo.Boolean 
import qualified CO4.Algorithms.Eitherize.IndexedGadt as IG
import           CO4.Algorithms.Eitherize.IndexedGadt hiding (constructorArgument
                                                             ,constructorArguments)
import           CO4.Util (maximumBy',replaceAt,toBinary,fromBinary,binaries,bitWidth)

--import Debug.Trace

data EncodedAdt =
    EncAdt { bits    :: [Boolean]
           , indexed :: IndexedGadt
           }
  | EncUndefined 

instance Show EncodedAdt where
  show (EncAdt bs ix) = concat $ [ "EncAdt { bits = ["
                                 -- , show $ replicate (length bs) "?"
                                 , showBooleans bs
                                 , "]"
                                 , ", indexed = ", show ix, " }"
                                 ]
  show EncUndefined   = "EncUndefined"

showBoolean :: Boolean -> String
showBoolean (Constant c) = show c
showBoolean (Boolean  b) = show b

showBooleans :: [Boolean] -> String
showBooleans = intercalate ", " . map showBoolean

instance Indexed EncodedAdt where
  index _ EncUndefined = error "EncodedAdt.index: EncUndefined"
  index n encAdt       = index n $ indexed encAdt

unknown :: MonadSAT m => IndexedGadt -> m EncodedAdt
unknown indexed = do
  bits <- sequence $ replicate (gadtWidth indexed) boolean
  mapM_ (excludeUndefinedGadtPath bits) undef
  excludeNonConsPaths bits indexed 
  return {- trace ("Excluded " ++ show (length undef) ++ " undefined path(s)") -}
         $ EncAdt bits indexed
  where 
    undef                         = undefinedGadtPaths indexed
    excludeUndefinedGadtPath bits = assert . concatMap nodeFlags
      where 
        nodeFlags (index,consIx) = 
          zipWith antiSelectBit (toBinary (length bits') consIx) bits'
          where bits'            = bits `atIndex` index

    excludeNonConsPaths bits indexed = do
      mapM_ (excludePattern bits') nonConsPatterns
      mapM_ (excludeNonConsPaths bits) $ concat $ catMaybes 
                                       $ IG.allConstructorArguments indexed
      where
        bits'           = bits `atIndex` (flagIndex indexed)
        nonConsPatterns = drop (IG.numConstructors indexed)
                        $ binaries $ IG.width $ IG.flagIndex indexed

    antiSelectBit True  bit = not bit
    antiSelectBit False bit = bit

flags :: EncodedAdt -> [Boolean]
flags EncUndefined = error "EncodedAdt.flags: EncUndefined"
flags adt          = bits adt `atIndex` (flagIndex $ indexed adt)

switchBy :: MonadSAT m => EncodedAdt -> [EncodedAdt] -> m EncodedAdt
switchBy adt branches = 
  if allBranchesUndefined -- !
  then return EncUndefined
  else do
    bits' <- premiseAndBranchBits >>= mkBits . equalWidth 
    return $ EncAdt bits' $ mergedIndices branches
  where
    allBranchesUndefined = all (P.not . isDefined) branches

    mkBits :: MonadSAT m => [(Boolean,[Boolean])] -> m [Boolean]
    mkBits premiseAndBranchBits = forM bits' $ \c -> mkImplications c >>= and
      where
        bits'          = transpose $ map snd premiseAndBranchBits
        premises       = map fst premiseAndBranchBits
        mkImplications = mapM (uncurry implies) . zip premises

    equalWidth :: [(Boolean,[Boolean])] -> [(Boolean,[Boolean])]
    equalWidth premiseAndBranchBits =
      let maxWidth = length $ snd $ maximumBy' (length . snd) premiseAndBranchBits
          
          padding (premise,bits) = 
            (premise, bits ++ replicate (maxWidth - (length bits)) (Constant False))
      in
        map padding premiseAndBranchBits
    
    mergedIndices = foldl1 merge . map indexed . filter isDefined

    premiseAndBranchBits :: MonadSAT m => m [(Boolean,[Boolean])]
    premiseAndBranchBits = zipWithM toBranch patterns branches 
      where 
        patterns                = binaries $ length $ flags adt
        selectFlag True  flag   = flag
        selectFlag False flag   = not flag

        toBranch pattern branch = do
          premise <- and $ zipWith selectFlag pattern $ flags adt
          return $ if isDefined branch 
                   then (premise, bits branch)
                   else (premise, [])

excludePattern :: MonadSAT m => [Boolean] -> [Bool] -> m ()
excludePattern flags pattern = Exception.assert (length flags == length pattern)
                             $ assert $ zipWith antiSelectBit flags pattern
  where 
    antiSelectBit bit True  = not bit
    antiSelectBit bit False = bit

encodedConsCall :: Int -> Int -> [EncodedAdt] -> EncodedAdt
encodedConsCall index numCons args = Exception.assert (index < numCons) $ 
  if containsUndefinedArgs
  then EncUndefined
  else EncAdt bits' $ indexedGadt 0 allArgs
  where
    bits'                 = if numCons > 1
                            then map Constant binIndex ++ (concatMap bits args)
                            else                           concatMap bits args
    binIndex              = toBinary (bitWidth numCons) index 
    allArgs               = replaceAt index (Just $ map IndexedWrapper args) 
                          $ replicate numCons Nothing
    containsUndefinedArgs = any (P.not . isDefined) args

constructorArgument :: Int -> Int -> EncodedAdt -> EncodedAdt
constructorArgument i j adt = case adt of
  EncUndefined -> EncUndefined
  EncAdt bs ix -> case IG.constructorArgument i j ix of
    Nothing  -> EncUndefined
    Just ix' -> EncAdt (bs `atIndex` (indexOfGadt ix')) $ normalize ix'

-- |The construction of an intermediate ADT simplifies the derivation of the
-- actual @Decode@ instance.
data IntermediateAdt = IntermediateConstructorIndex Int [EncodedAdt]
                     | IntermediateUndefined
                     deriving Show

toIntermediateAdt :: (MonadSAT m, Decode m Boolean Bool) => EncodedAdt -> m IntermediateAdt 
toIntermediateAdt adt = case adt of
  EncUndefined                 -> return IntermediateUndefined
  EncAdt {} | null (flags adt) -> return $ intermediate 0
  EncAdt {}                    -> 
    return (intermediate . fromBinary) `ap` decode (flags adt)

  where 
    intermediate :: Int -> IntermediateAdt
    intermediate i = maybe IntermediateUndefined
                     ( IntermediateConstructorIndex i . map (EncAdt $ bits adt) )  
                   $ IG.constructorArguments i $ indexed adt

isDefined :: EncodedAdt -> Bool
isDefined EncUndefined = False
isDefined _            = True
