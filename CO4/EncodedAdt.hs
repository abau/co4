{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module CO4.EncodedAdt
  ( Primitive, EncodedAdt, IntermediateAdt (..)
  , make, encUndefined, encBottom, encodedConstructor
  , isBottom, isDefined, isUndefined, isConstantlyDefined, isConstantlyUndefined
  , isValid, isInvalid
  , flags, flags', constantConstructorIndex, definedness
  , arguments, arguments', constructorArgument
  , caseOf, toIntermediateAdt, caseOfBits
  )
where

import           Prelude hiding (and,undefined)
import qualified Prelude
import qualified Control.Exception as Exception
import           Control.Monad (forM)
import           Data.List (transpose)
import           Data.Maybe (fromMaybe,catMaybes,fromJust)
import           Data.Tree (Tree (..),drawTree)
import           Satchmo.Core.Primitive (constant,select,primitive,assert,and)
import qualified Satchmo.Core.Primitive as P
import           Satchmo.Core.Decode (Decode,decode)
import           CO4.Monad 
import           CO4.Util (bitWidth,binaries,for,fromBinary,toBinary)
import           CO4.EncodedAdtData (Primitive,EncodedAdt (..))

data IntermediateAdt = IntermediateConstructorIndex Int [EncodedAdt]
                     | IntermediateUndefined

instance Show EncodedAdt where
  show = drawTree . toTree 
    where
      toTree adt | isConstantlyUndefined adt = Node "undefined" []
      toTree (EncodedAdt id def fs conss)      = 
        Node (concat [ "id: ", show id
                     , ", definedness: ", show def
                     , ", flags: ", show fs]) 
             (map toTree conss)
      toTree Bottom = Node "bottom" [] 

-- * Constructors

make :: Primitive -> [Primitive] -> [EncodedAdt] -> CO4 EncodedAdt
make definedness flags arguments = withAdtCache (definedness, flags, arguments)

encUndefined :: EncodedAdt
encUndefined = EncodedAdt (-1) (constant False) [] []

encBottom :: EncodedAdt
encBottom = Bottom

encodedConstructor :: Int -> Int -> [EncodedAdt] -> CO4 EncodedAdt
encodedConstructor i n args = Exception.assert (i < n) 
                            $ withAdtCache (constant True,flags,args)
  where
    flags = case n of 1 -> []
                      _ -> map constant $ toBinary (Just $ bitWidth n) i

-- * Predicates 
isBottom :: EncodedAdt -> Bool
isBottom = \case Bottom -> True
                 _      -> False

isDefined,isUndefined :: EncodedAdt -> Maybe Bool
isDefined   = P.evaluateConstant . definedness
isUndefined = fmap not . isDefined

isConstantlyDefined,isConstantlyUndefined :: EncodedAdt -> Bool
isConstantlyDefined   = fromMaybe False . isDefined
isConstantlyUndefined = fromMaybe False . isUndefined

-- |`isValid x = not ( isConstantlyUndefined x || isBottom x )`
isValid :: EncodedAdt -> Bool
isValid x = not ( isConstantlyUndefined x || isBottom x )

isInvalid :: EncodedAdt -> Bool
isInvalid = not . isValid

-- * Accessors

flags :: EncodedAdt -> Maybe [Primitive]
flags = \case Bottom -> Nothing
              adt    -> if isConstantlyUndefined adt
                        then Nothing
                        else Just $ _flags adt

-- |Unsafe version of `flags`
flags' :: EncodedAdt -> [Primitive]
flags' e = case flags e of
  Nothing -> error "EncodedAdt.flags': missing flags"
  Just fs -> fs

constantConstructorIndex :: EncodedAdt -> Maybe Int
constantConstructorIndex adt = case flags adt of
  Nothing -> error "EncodedAdt.constantConstructorIndex: no flags"
  Just [] -> Just 0
  Just fs -> primitivesToDecimal fs

definedness :: EncodedAdt -> Primitive
definedness = \case Bottom -> constant True
                    adt    -> _definedness adt

arguments :: EncodedAdt -> Maybe [EncodedAdt]
arguments adt | isInvalid adt = Nothing
arguments adt                 = Just $ _arguments adt 

-- |Unsafe version of `arguments`
arguments' :: EncodedAdt -> [EncodedAdt]
arguments' adt = case arguments adt of
  Nothing   -> error "EncodedAdt.arguments': missing arguments"
  Just args -> args

constructorArgument :: Int -> Int -> EncodedAdt -> EncodedAdt
constructorArgument _ _ adt | isConstantlyUndefined adt = encUndefined
constructorArgument _ _ adt | isBottom adt              = encBottom
constructorArgument i j adt = 
  case constantConstructorIndex adt of
    Nothing           -> -- Exception.assert (i < length args) $ args !! i
                         if i < length args then args !! i
                                            else encBottom
    Just j' | j == j' -> Exception.assert (i < length args) $ args !! i
    Just _            -> encBottom
  where
    args = _arguments adt

-- * Utilities

-- |Case distinction between encoded ADTs
caseOf :: EncodedAdt -> [EncodedAdt] -> CO4 EncodedAdt
caseOf adt branches | isConstantlyUndefined adt 
                   || (all isConstantlyUndefined branches) 
                   || (all (\x -> isConstantlyUndefined x || isBottom x) branches)
                    = return encUndefined
caseOf adt branches | isBottom adt || (all isBottom branches)
                    = return Bottom
caseOf adt branches | length (fromJust $ flags adt) < bitWidth (length branches) 
                    = return Bottom --error "EncodedAdt.Overlapping.caseOf: missing flags"
caseOf adt branches =
  case constantConstructorIndex adt of
    Just i  -> Exception.assert (i < length branches) $ return $ branches !! i
    Nothing -> do 
      [branchDef] <- caseOfBits relevantFlags 
                   $ map (Just . return . definedness)        branches

      def'        <- and [branchDef, definedness adt]
      id'         <- newId

      let adt'    = EncodedAdt id' def' relevantFlags (fromJust $ arguments adt)

      flags'      <- caseOfBits relevantFlags $ map flags     branches
      arguments'  <- caseOfArguments adt'     $ map arguments branches
      return $ EncodedAdt id' def' flags' arguments'
  where
    relevantFlags = take (bitWidth $ length branches) $ fromJust $ flags adt

-- |Case distinction between encoded arguments of ADTs
caseOfArguments :: EncodedAdt -> [Maybe [EncodedAdt]] -> CO4 [EncodedAdt]
caseOfArguments adt branchArguments = 
  forM (transpose sameSizeBranchArguments) $ caseOf adt
  where 
    sameSizeBranchArguments = for branchArguments $ \case 
      Nothing   -> replicate maxArgs Bottom
      Just args -> args ++ replicate (maxArgs - length args) Bottom

    maxArgs = maximum $ map length $ catMaybes branchArguments

toIntermediateAdt :: (Decode m Primitive Bool) => EncodedAdt -> Int -> m IntermediateAdt
toIntermediateAdt adt _ | isConstantlyUndefined adt = return IntermediateUndefined 
toIntermediateAdt (EncodedAdt _ definedness flags args) n = 
  Exception.assert (length flags >= bitWidth n) $ do
    decode definedness >>= \case 
      False -> return IntermediateUndefined
      True  -> if null relevantFlags 
               then return $ intermediate 0
               else decode relevantFlags >>= return . intermediate . fromBinary
        where
          relevantFlags  = take (bitWidth n) flags
          intermediate i = IntermediateConstructorIndex i args 

primitivesToDecimal :: [Primitive] -> Maybe Int
primitivesToDecimal ps = 
  if all P.isConstant ps
  then Just $ fromBinary $ map (fromJust . P.evaluateConstant) ps
  else Nothing

caseOfBits :: [Primitive] -> [Maybe [Primitive]] -> CO4 [Primitive]
caseOfBits flags branchBits = 
    Exception.assert (not $ null nonBottomBits) 
  $ Exception.assert (length flags == bitWidth (length branchBits)) 
  $ case all equalBits (transpose branchBits') of
      True  -> return $ head $ branchBits'
      False -> case primitivesToDecimal flags of
        Nothing -> forM (transpose branchBits') mergeN 
        Just i  -> return $ branchBits' !! i
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
