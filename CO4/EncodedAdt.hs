{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module CO4.EncodedAdt
  ( Primitive, EncodedAdt, IntermediateAdt (..)
  , make, makeWithStackTrace, encUndefined, encEmpty, encodedConstructor
  , isEmpty, isDefined, isUndefined, isConstantlyDefined, isConstantlyUndefined
  , isValid, isInvalid
  , flags, flags', constantConstructorIndex, definedness
  , arguments, arguments', constructorArgument, origin
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
import           Text.PrettyPrint (Doc,(<+>),vcat,int,text,nest,empty,($$))
import           Satchmo.Core.Primitive (constant,select,primitive,assert,and)
import qualified Satchmo.Core.Primitive as P
import           Satchmo.Core.Boolean (Boolean)
import           Satchmo.Core.Decode (Decode,decode)
import           CO4.Monad 
import           CO4.Util (bitWidth,binaries,for,fromBinary,toBinary)
import           CO4.Stack (StackTrace)

-- See EncodedAdt.hs-boot
type Primitive = Boolean

data EncodedAdt = EncodedAdt { _id          :: ! Int
                             , _definedness :: ! Primitive
                             , _flags       :: ! [Primitive] 
                             , _arguments   :: ! [EncodedAdt] 
                             , _origin      :: ! Doc
                             }
                | Empty

instance Eq EncodedAdt where
  Empty == Empty = True
  _     == Empty = False
  Empty == _     = False
  a     == b     = _id a == _id b

instance Ord EncodedAdt where
  compare Empty  Empty  = EQ
  compare _      Empty  = GT
  compare Empty  _      = LT
  compare a      b      = compare (_id a) (_id b)

data IntermediateAdt = IntermediateConstructorIndex Int [EncodedAdt]
                     | IntermediateUndefined
                     | IntermediateEmpty

instance Show EncodedAdt where
  show = drawTree . toTree 
    where
      toTree adt | isConstantlyUndefined adt = Node "undefined" []
      toTree (EncodedAdt id def fs conss o)      = 
        Node (concat [ "id: ", show id
                     , ", definedness: ", show def
                     , ", flags: ", show fs
                     , ", origin: ", show o]
                     ) 
             (map toTree conss)
      toTree Empty  = Node "empty" [] 

-- * Constructors

make :: Primitive -> [Primitive] -> [EncodedAdt] -> CO4 EncodedAdt
make definedness flags arguments = withAdtCache (definedness, flags, arguments)

makeWithStackTrace :: Int -> Primitive -> [Primitive] -> [EncodedAdt] -> StackTrace
                   -> EncodedAdt
makeWithStackTrace i d f a o = EncodedAdt i d f a $ vcat $ map text o


encUndefined :: EncodedAdt
encUndefined = makeWithStackTrace (-1) (constant False) [] [] ["undefined"]

encEmpty :: EncodedAdt
encEmpty = Empty

encodedConstructor :: Integral i => i -> i -> [EncodedAdt] -> CO4 EncodedAdt
encodedConstructor i n args = Exception.assert (i < n) 
                            $ withAdtCache (constant True,flags,args)
  where
    flags = case n of 1 -> []
                      _ -> map constant $ toBinary (Just $ bitWidth n) i

-- * Predicates 
isEmpty :: EncodedAdt -> Bool
isEmpty = \case Empty -> True
                _     -> False

isDefined,isUndefined :: EncodedAdt -> Maybe Bool
isDefined   = P.evaluateConstant . definedness
isUndefined = fmap not . isDefined

isConstantlyDefined,isConstantlyUndefined :: EncodedAdt -> Bool
isConstantlyDefined   = fromMaybe False . isDefined
isConstantlyUndefined = fromMaybe False . isUndefined

-- |`isValid x = not ( isConstantlyUndefined x || isEmpty x )`
isValid :: EncodedAdt -> Bool
isValid x = not ( isConstantlyUndefined x || isEmpty x )

isInvalid :: EncodedAdt -> Bool
isInvalid = not . isValid

-- * Accessors

flags :: EncodedAdt -> Maybe [Primitive]
flags = \case Empty -> Nothing
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
definedness = \case Empty -> constant True
                    adt   -> _definedness adt

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
constructorArgument _ _ adt | isEmpty adt              = encEmpty
constructorArgument i j adt = 
  case constantConstructorIndex adt of
    Nothing           -> -- Exception.assert (i < length args) $ args !! i
                         if i < length args then args !! i
                                            else encEmpty
    Just j' | j == j' -> Exception.assert (i < length args) $ args !! i
    Just _            -> encEmpty
  where
    args = _arguments adt

origin :: EncodedAdt -> Doc
origin Empty = text "empty"
origin adt    = _origin adt

-- * Utilities

-- |Case distinction between encoded ADTs
caseOf :: EncodedAdt -> [EncodedAdt] -> CO4 EncodedAdt
caseOf adt branches | isConstantlyUndefined adt 
                   || (all isConstantlyUndefined branches) 
                   || (all (\x -> isConstantlyUndefined x || isEmpty x) branches)
                    = return encUndefined
caseOf adt branches | isEmpty adt || (all isEmpty branches)
                    = return Empty
caseOf adt branches | length (fromJust $ flags adt) < bitWidth (length branches) 
                    = return Empty --error "EncodedAdt.Overlapping.caseOf: missing flags"
caseOf adt branches =
  case constantConstructorIndex adt of
    Just i  -> Exception.assert (i < length branches) $ return $ branches !! i
    Nothing -> do 
      [branchDef] <- caseOfBits relevantFlags 
                   $ map (Just . return . definedness)        branches

      def'        <- and [branchDef, definedness adt]
      id'         <- newId
      origin'     <- mergeOrigins branches

      let adt'    = EncodedAdt id' def' relevantFlags (fromJust $ arguments adt) origin'

      flags'      <- caseOfBits relevantFlags $ map flags     branches
      arguments'  <- caseOfArguments adt'     $ map arguments branches

      return $ EncodedAdt id' def' flags' arguments' origin'
  where
    relevantFlags = take (bitWidth $ length branches) $ fromJust $ flags adt

mergeOrigins :: [EncodedAdt] -> CO4 Doc
mergeOrigins branches =
  isProfileRun >>= \case
    False -> return empty
    True  -> do
      trace <- getStackTrace
      return $ merged trace
      where
        merged trace = vcat $
            (text "merge")
          : (nest 2 $ vcat $ (text "merge-trace") : map (nest 2 . text) trace)
          : zipWith (\i branch -> nest 2 
                                $ (text "merge-branch" <+> int i)
                               $$ (nest 2 $ origin branch)
                    ) [0..] branches

-- |Case distinction between encoded arguments of ADTs
caseOfArguments :: EncodedAdt -> [Maybe [EncodedAdt]] -> CO4 [EncodedAdt]
caseOfArguments adt branchArguments = 
  forM (transpose sameSizeBranchArguments) $ caseOf adt
  where 
    sameSizeBranchArguments = for branchArguments $ \case 
      Nothing   -> replicate maxArgs Empty
      Just args -> args ++ replicate (maxArgs - length args) Empty

    maxArgs = maximum $ map length $ catMaybes branchArguments

toIntermediateAdt :: (Decode m Primitive Bool) => EncodedAdt -> Int -> m IntermediateAdt
toIntermediateAdt adt _ | isConstantlyUndefined adt = return IntermediateUndefined 
toIntermediateAdt Empty _                          = return IntermediateEmpty
toIntermediateAdt (EncodedAdt _ definedness flags args _) n = 
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
    Exception.assert (not $ null nonEmptyBits) 
  $ Exception.assert (length flags == bitWidth (length branchBits)) 
  $ case all equalBits (transpose branchBits') of
      True  -> return $ head $ branchBits'
      False -> case primitivesToDecimal flags of
        Nothing -> forM (transpose branchBits') mergeN 
        Just i  -> return $ branchBits' !! i
    where
      nonEmptyBits   = catMaybes branchBits
      branchBitWidth = maximum $ map length nonEmptyBits 
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
