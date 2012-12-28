{-# language MultiParamTypeClasses #-}
module CO4.EncodedAdt
  ( EncodedAdt (EncUndefined), unknown, flags, switchBy, encodedConsCall
  , constructorArgument
  , IntermediateAdt (..), toIntermediateAdt
  )
where

import           Prelude hiding (not,or,and)
import qualified Prelude as P
import           Control.Monad (ap,forM,zipWithM,liftM)
import qualified Control.Exception as Exception 
import           Data.List (transpose)
import           Data.Maybe (catMaybes)
import           Satchmo.SAT.Mini (SAT)
import           Satchmo.Code 
import           Satchmo.Boolean 
import qualified CO4.Algorithms.Eitherize.IndexedGadt as IG
import           CO4.Algorithms.Eitherize.IndexedGadt hiding (constructorArgument
                                                             ,constructorArguments)
import           CO4.Util (maximumBy',replaceAt,toBinary,fromBinary,binaries)

import Debug.Trace

data EncodedAdt =
    EncAdt { bits    :: [Boolean]
           , indexed :: IndexedGadt
           }
  | EncUndefined 

instance Show EncodedAdt where
  show (EncAdt bs ix) = concat $ [ "EncAdt { bits = "
                                 , show $ replicate (length bs) "?"
                                 , ", indexed = ", show ix, " }"]

instance Indexed EncodedAdt where
  index n = index n . indexed


unknown :: IndexedGadt -> SAT EncodedAdt
unknown indexed = do
  bits <- sequence $ replicate (gadtWidth indexed) boolean
  mapM_ (excludeUndefinedGadtPath bits) undef
  return $ trace ("Excluded " ++ show (length undef) ++ " path(s)") 
         $ EncAdt bits indexed
  where 
    undef                         = undefinedGadtPaths indexed
    excludeUndefinedGadtPath bits = assert . concatMap nodeFlags
      where 
        nodeFlags (index,consIx) = 
          zipWith antiSelectBit (toBinary (length bits') consIx) bits'
          where bits'            = bits `atIndex` index

        antiSelectBit True  bit = not bit
        antiSelectBit False bit = bit

flags :: EncodedAdt -> [Boolean]
flags adt = bits adt `atIndex` (flagIndex $ indexed adt)

switchBy :: EncodedAdt -> [EncodedAdt] -> SAT EncodedAdt
switchBy adt branches = do
  branches' <- encodedBranches
  if null branches'
    then return EncUndefined
    else do
      bits' <- mkBits $ equalWidthBranches branches' 
      return $ EncAdt bits' $ mergedIndices branches'
  where
    mkBits :: [EncodedBranch] -> SAT [Boolean]
    mkBits encBranches = forM columns $ \c -> mkImplications c >>= and
      where
        columns        = transpose $ map (bits . branchAdt) encBranches
        premises       = map branchPremise encBranches
        mkImplications = mapM (uncurry implies) . zip premises

    equalWidthBranches :: [EncodedBranch] -> [EncodedBranch]
    equalWidthBranches encBranches =
      let widthestBranch = maximumBy' (gadtWidth . indexed . branchAdt) encBranches
          maxWidth       = gadtWidth $ indexed $ branchAdt widthestBranch
          padding (DefinedBranch pattern branch premise) = 
            let n = maxWidth - (gadtWidth $ indexed branch)
            in
              DefinedBranch pattern
                (branch { bits = bits branch ++ replicate n (Constant False)}) premise
      in
        map padding encBranches
    
    mergedIndices   = foldl1 merge . map (indexed . branchAdt)

    encodedBranches = liftM catMaybes $ zipWithM toBranch patterns branches 
      where 
        patterns                = binaries $ length $ flags adt
        selectFlag True  flag   = flag
        selectFlag False flag   = not flag

        toBranch pattern branch = 
          if isDefined branch 
          then do premise <- and $ zipWith selectFlag pattern $ flags adt
                  return $ Just $ DefinedBranch pattern branch premise

          else do excludePattern (flags adt) pattern
                  return Nothing

excludePattern :: [Boolean] -> [Bool] -> SAT ()
excludePattern flags pattern = Exception.assert (length flags == length pattern)
                             $ assert $ zipWith antiSelectBit flags pattern
  where 
    antiSelectBit bit True  = not bit
    antiSelectBit bit False = bit

encodedConsCall :: Int -> Int -> [EncodedAdt] -> EncodedAdt
encodedConsCall index numCons args = Exception.assert (index < numCons) 
                                   $ EncAdt bits'
                                   $ indexedGadt 0 allArgs
  where
    bits'         = map Constant binIndex ++ (concatMap bits args)
    allArgs       = replaceAt index (Just $ map IndexedWrapper args) 
                  $ replicate numCons Nothing

    binIndex      = toBinary binIndexWidth index
    binIndexWidth = ceiling $ logBase 2 $ fromIntegral numCons

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

toIntermediateAdt :: EncodedAdt -> SAT IntermediateAdt 
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

data EncodedBranch = DefinedBranch { branchPattern :: [Bool]
                                   , branchAdt     :: EncodedAdt
                                   , branchPremise :: Boolean
                                   }
