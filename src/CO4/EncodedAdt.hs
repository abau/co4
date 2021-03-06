{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module CO4.EncodedAdt
  ( Primitive, EncodedAdt, IntermediateAdt (..)
  , make, makeWithId, encEmpty, encodedConstructor
  , isEmpty, isValid, isInvalid
  , id, id', flags, flags', constantConstructorIndex
  , arguments, arguments', isPrefixfree, isPrefixfree', constructorArgument
  , onValidDiscriminant, ifReachable, allBranchesPrefixfree, caseOf, toIntermediateAdt, trimFlags
  )
where

import           Prelude hiding (and,undefined,id)
import qualified Prelude
import qualified Control.Exception as Exception
import           Control.Monad (forM)
import           Data.Function (on)
import           Data.List (transpose,genericIndex,genericLength,maximumBy)
import           Data.Maybe (catMaybes,fromJust)
import           Data.Tree (Tree (..),drawTree)
import           Satchmo.Core.Primitive (constant,select,primitive,assert)
import qualified Satchmo.Core.Primitive as P
import           Satchmo.Core.Boolean (Boolean)
import           Satchmo.Core.Decode (Decode,decode)
import           CO4.Monad 
import           CO4.Util (for)
import           CO4.Prefixfree (numeric,invNumeric,discriminates)

-- See EncodedAdt.hs-boot
type Primitive = Boolean

data EncodedAdt = EncodedAdt { _id           :: ! Int
                             , _flags        :: ! [Primitive] 
                             , _arguments    :: ! [EncodedAdt] 
                             , _isPrefixfree :: ! Bool
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
                     | IntermediateEmpty

instance Show EncodedAdt where
  show = drawTree . toTree 
    where
      toTree (EncodedAdt id fs conss pf) = 
        Node (concat [ "id: "           , show id
                     , ", flags: "      , show fs
                     , ", prefixfree: " , show pf
                     ]
             ) 
             (map toTree conss)
      toTree Empty  = Node "empty" [] 

-- * Constructors

make :: [Primitive] -> [EncodedAdt] -> Bool -> CO4 EncodedAdt
make flags arguments pfFree = withAdtCache (flags, arguments, pfFree)

makeWithId :: Int -> [Primitive] -> [EncodedAdt] -> Bool -> EncodedAdt
makeWithId = EncodedAdt

encEmpty :: EncodedAdt
encEmpty = Empty

encodedConstructor :: Int -> Int -> [EncodedAdt] -> CO4 EncodedAdt
encodedConstructor i n args = Exception.assert (i < n) 
                            $ withAdtCache (flags,args,True)
  where
    flags = case n of 1 -> []
                      _ -> map constant $ invNumeric n i

-- * Predicates 
isEmpty :: EncodedAdt -> Bool
isEmpty = \case Empty -> True
                _     -> False

-- |`isValid x = not ( isEmpty x )`
isValid :: EncodedAdt -> Bool
isValid x = not ( isEmpty x )

isInvalid :: EncodedAdt -> Bool
isInvalid = not . isValid

-- * Accessors

id :: EncodedAdt -> Maybe Int
id = \case Empty -> Nothing
           adt   -> Just $ _id adt

id' :: EncodedAdt -> Int
id' e = case id e of
  Nothing -> error "EncodedAdt.id': missing id"
  Just id -> id

flags :: EncodedAdt -> Maybe [Primitive]
flags = \case Empty -> Nothing
              adt   -> Just $ _flags adt

-- |Unsafe version of `flags`
flags' :: EncodedAdt -> [Primitive]
flags' e = case flags e of
  Nothing -> error "EncodedAdt.flags': missing flags"
  Just fs -> fs

constantConstructorIndex :: Int -> EncodedAdt -> Maybe Int
constantConstructorIndex n adt = case flags adt of
  Nothing -> error "EncodedAdt.constantConstructorIndex: missing flags"
  Just fs -> primitivesToDecimal n fs

arguments :: EncodedAdt -> Maybe [EncodedAdt]
arguments adt | isInvalid adt = Nothing
arguments adt                 = Just $ _arguments adt 

isPrefixfree :: EncodedAdt -> Maybe Bool
isPrefixfree = \case 
  Empty -> Nothing
  adt   -> Just $ _isPrefixfree adt

isPrefixfree' :: EncodedAdt -> Bool
isPrefixfree' adt = case isPrefixfree adt of
  Nothing -> error "EncodedAdt.isPrefixfree': empty"
  Just pf -> pf

-- |Unsafe version of `arguments`
arguments' :: EncodedAdt -> [EncodedAdt]
arguments' adt = case arguments adt of
  Nothing   -> error "EncodedAdt.arguments': missing arguments"
  Just args -> args

constructorArgument :: Int -> Int -> Int -> EncodedAdt -> EncodedAdt
constructorArgument _ _ _ adt | isEmpty adt = encEmpty
constructorArgument n i j adt = 
  case constantConstructorIndex n adt of
    Nothing           -> if i < genericLength args 
                         then args `genericIndex` i
                         else encEmpty
    Just j' | j == j' -> Exception.assert (i < genericLength args) 
                       $ args `genericIndex` i
    Just _            -> encEmpty
  where
    args = _arguments adt

-- * Utilities

-- |@onValidDiscriminant d n f@
--  * returns 'Empty', if @d@ is empty or @discriminates n (flags' d) == False@
--  * returns @f@ otherwise
onValidDiscriminant :: EncodedAdt -> Int -> CO4 EncodedAdt -> CO4 EncodedAdt
onValidDiscriminant d n f = 
  if isEmpty d then return encEmpty
  else if discriminates n (flags' d) 
       then f
       else return encEmpty

-- |@isValidDiscriminant d n@ checks if @d@ is a valid discriminant, i.e.
--  * if @d@ is not empty
--  * if @discriminates n (flags' d) == True@
isValidDiscriminant :: EncodedAdt -> Int -> Bool
isValidDiscriminant adt n = Prelude.and [ not $ isEmpty               adt
                                        , discriminates n (flags' adt)
                                        ]

-- |@ifReachable d i n b@ evaluates the @i@-th branch @b@ of a case-distinction with @n@
-- constructors on discriminat @d@, if the @i@-th branch is reachable according to @d@, i.e.
-- if @d@ is not constant or if @d@ is constant @i@.
ifReachable :: EncodedAdt -> Int -> Int -> CO4 EncodedAdt -> CO4 EncodedAdt
ifReachable d i n b = Exception.assert (isValidDiscriminant d n) $
  case primitivesToDecimal n $ flags' d of
    Nothing         -> b
    Just j | i == j -> b
    Just _          -> return encEmpty

-- |Holds if branch flags are encoded using a prefixfree encoding
allBranchesPrefixfree :: [EncodedAdt] -> Bool
allBranchesPrefixfree branches = case catMaybes (map isPrefixfree branches) of
  [] -> error "EncodedAdt.allBranchesPrefixfree: no non-empty branch present"
  bs -> Prelude.and bs

-- |Case distinction between encoded ADTs
caseOf :: EncodedAdt -> [EncodedAdt] -> CO4 EncodedAdt
caseOf adt branches | isEmpty adt || (all isEmpty branches)
                    = return Empty
caseOf adt branches | not (discriminates (genericLength branches) (flags' adt))
                    = error "EncodedAdt.caseOf: missing flags (use 'onValidDiscriminant')"
caseOf adt branches =
  case constantConstructorIndex numCons adt of
    Just i  -> Exception.assert (i < genericLength branches) 
             $ return $ branches `genericIndex` i
    Nothing -> do 
      id'        <- newId

      let adt'   = EncodedAdt id' fs (fromJust $ arguments adt) allPfFree

      flags'     <- caseOfBits allPfFree fs $ map flags branches
      arguments' <- caseOfArguments adt' $ map arguments branches

      return $ EncodedAdt id' flags' arguments' allPfFree
  where
    numCons   = genericLength branches
    fs        = flags' adt
    allPfFree = allBranchesPrefixfree branches

-- |Case distinction between encoded arguments of ADTs
caseOfArguments :: EncodedAdt -> [Maybe [EncodedAdt]] -> CO4 [EncodedAdt]
caseOfArguments adt branchArguments = 
  forM (transpose sameSizeBranchArguments) $ caseOf adt
  where 
    sameSizeBranchArguments = for branchArguments $ \case 
      Nothing   -> replicate maxArgs Empty
      Just args -> args ++ replicate (maxArgs - length args) Empty

    maxArgs = maximum $ map length $ catMaybes branchArguments

toIntermediateAdt :: (Decode m Primitive Bool) 
                  => EncodedAdt -> Int -> m IntermediateAdt
toIntermediateAdt Empty _                          = return IntermediateEmpty
toIntermediateAdt (EncodedAdt _ _ _ False) _       = error "EncodedAdt.toIntermediateAdt: ADTs must be encoded using prefixfree encoding"
toIntermediateAdt (EncodedAdt _ flags args True) n =
  decode flags >>= return . intermediate . numeric n
    where
      intermediate i = IntermediateConstructorIndex i args

primitivesToDecimal :: Int -> [Primitive] -> Maybe Int
primitivesToDecimal n ps = 
  if all P.isConstant ps
  then Just $ numeric n $ map (fromJust . P.evaluateConstant) ps
  else Nothing

caseOfBits :: Bool -> [Primitive] -> [Maybe [Primitive]] -> CO4 [Primitive]
caseOfBits prefixfree flags branchBits = 
    Exception.assert (not $ null nonEmptyBits) 
  $ Exception.assert (discriminates numCons flags) 
  $ Exception.assert (primitivesToDecimal numCons flags == Nothing)
  $ forM (transpose branchBits') mergeN 
    where
      numCons             = genericLength branchBits
      nonEmptyBits        = catMaybes branchBits
      longestNonEmptyBits = maximumBy (compare `on` length) nonEmptyBits
      branchWidth         = length longestNonEmptyBits
      branchBits'         = for branchBits $ \case
        Nothing | prefixfree -> longestNonEmptyBits
        Nothing              -> replicate branchWidth $ constant False
        Just bs | prefixfree -> bs ++ (drop (length bs) longestNonEmptyBits)
        Just bs              -> bs ++ (replicate (branchWidth - length bs) $ constant False)

      equalBits bs = all (\b -> b == head bs) bs

      mergeN bitsT = case equalBits bitsT of
        True  -> return $ head bitsT 
        False -> do
           r <- primitive
           forM (zip bitsT [0..]) $ \ (b, i) -> do
                let pattern = invNumeric numCons i
                    fs      = zipWith select pattern flags
                    
                -- fs => (r <=> b)
                assert ( r : P.not b : map P.not fs  )
                assert ( P.not r :  b : map P.not fs  )
           return r

trimFlags :: Int -> EncodedAdt -> EncodedAdt
trimFlags n adt = case flags adt of
  Nothing -> adt
  Just fs -> adt { _flags = take n fs }
