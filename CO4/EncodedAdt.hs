{-# language MultiParamTypeClasses #-}
module CO4.EncodedAdt
  ( EncodedAdt (EncUndefined), unknown, flags, switchBy, encodedConsCall
  , constructorArgument
  , IntermediateAdt (..), toIntermediateAdt
  )
where

import           Prelude hiding (not,or,and)
import qualified Prelude as P
import           Control.Monad (ap,forM)
import qualified Control.Exception as Exception 
import           Data.List (transpose)
import           Satchmo.SAT.Mini (SAT)
import           Satchmo.Code 
import           Satchmo.Boolean 
import qualified CO4.Algorithms.Eitherize.IndexedGadt as IG
import           CO4.Algorithms.Eitherize.IndexedGadt hiding (constructorArgument
                                                             ,constructorArguments)
import           CO4.Util (maximumBy',replaceAt)

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
  excludeUndefinedFlags bits $ undefinedConstructors indexed
  return $ EncAdt bits indexed

flags :: EncodedAdt -> [Boolean]
flags adt = bits adt `atIndex` (flagIndex $ indexed adt)

switchBy :: EncodedAdt -> [EncodedAdt] -> SAT EncodedAdt
switchBy adt branches = Exception.assert areFlagsEqualWidth $
  if null definedBranches 
  then return EncUndefined
  else do
    premisses <- mkPremisses
    bits'     <- mkBits premisses
    excludeUndefinedFlags (bits adt) undefinedBranchIndices
    return $ EncAdt bits' mergedIndices
  where
    areFlagsEqualWidth = all (\b -> length (flags b) == n) definedBranches
      where n = length $ flags $ head definedBranches

    mkPremisses :: SAT [Boolean]
    mkPremisses = forM patternBitss $ and . mkPremiss 
      where
        patternBitss          = binaries $ length $ flags adt
        mkPremiss pattern     = zipWith selectFlag pattern $ flags adt
        selectFlag True  flag = flag
        selectFlag False flag = not flag

    mkBits :: [Boolean] -> SAT [Boolean]
    mkBits premisses = forM columns $ \c -> mkImplications c >>= and
      where
        columns        = transpose $ map bits equalWidthBranches
        mkImplications = mapM (uncurry implies) . zip premisses

    equalWidthBranches =
      let widthestBranch = maximumBy' (gadtWidth . indexed) definedBranches
          maxWidth       = gadtWidth $ indexed widthestBranch
          padding branch = 
            let n = maxWidth - (gadtWidth $ indexed branch)
            in
              branch { bits = bits branch ++ replicate n (Constant False) }
      in
        map padding definedBranches
    
    mergedIndices   = foldl1 merge $ map indexed definedBranches
    definedBranches = filter isDefined branches

    undefinedBranchIndices = map fst $ filter (P.not . isDefined . snd) 
                                     $ zip [0..] branches

excludeUndefinedFlags :: [Boolean] -> [Int] -> SAT ()
excludeUndefinedFlags flags = mapM_ (excludeIndex . toBinary (length flags))
  where 
    excludeIndex             = assert . map excludeBoolean . zip flags
    excludeBoolean (b,True)  = not b
    excludeBoolean (b,False) = b
                
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

toBinary :: Int -> Int -> [Bool]
toBinary n i = result ++ replicate (n - length result) False 
  where 
    result = go i
    go 0   = [False]
    go 1   = [True]
    go i   = case i `quotRem` 2 of
               (i',0) -> False : (go i')
               (i',1) -> True  : (go i')

fromBinary :: [Bool] -> Int
fromBinary = go 0
  where
    go i [True]     = 2^i 
    go _ [False]    = 0
    go i (True:xs)  = 2^i + go (i+1) xs
    go i (False:xs) =       go (i+1) xs

binaries :: Int -> [[Bool]]
binaries 1 = [[False],[True]]
binaries i = do
  y <- binaries $ i - 1
  x <- [False,True]
  return $ x : y

isDefined :: EncodedAdt -> Bool
isDefined EncUndefined = False
isDefined _            = True
