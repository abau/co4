{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module CO4.EncodedAdt
  ( Primitive, EncodedAdt, IntermediateAdt (..)
  , make, makeWithStackTrace, encUndefined, encEmpty, encodedConstructor
  , isEmpty, isDefined, isUndefined, isConstantlyDefined, isConstantlyUndefined
  , isValid, isInvalid
  , flags, flags', constantConstructorIndex, definedness
  , arguments, arguments', constructorArgument, origin
  , onValidDiscriminant, ifReachable, caseOf, toIntermediateAdt, caseOfBits, trimFlags
  )
where

import           Prelude hiding (and,undefined)
import qualified Prelude
import qualified Control.Exception as Exception
import           Control.Monad (forM)
import           Data.Function (on)
import           Data.List (transpose,genericIndex,genericLength,maximumBy)
import           Data.Maybe (fromMaybe,catMaybes,fromJust)
import           Data.Tree (Tree (..),drawTree)
import           Text.PrettyPrint (Doc,(<+>),vcat,int,text,nest,empty,($$))
import           Satchmo.Core.Primitive (constant,select,primitive,assert,and)
import qualified Satchmo.Core.Primitive as P
import           Satchmo.Core.Boolean (Boolean)
import           Satchmo.Core.Decode (Decode,decode)
import           CO4.Monad 
import           CO4.Util (bitWidth,for)
import           CO4.Stack (CallStackTrace)
import           CO4.Prefixfree (numeric,invNumeric,discriminates)

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

data IntermediateAdt = IntermediateConstructorIndex Integer [EncodedAdt]
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

makeWithStackTrace :: Int -> Primitive -> [Primitive] -> [EncodedAdt] -> CallStackTrace
                   -> EncodedAdt
makeWithStackTrace i d f a o = EncodedAdt i d f a $ vcat $ map text o


encUndefined :: EncodedAdt
encUndefined = makeWithStackTrace (-1) (constant False) [] [] ["undefined"]

encEmpty :: EncodedAdt
encEmpty = Empty

encodedConstructor :: Integer -> Integer -> [EncodedAdt] -> CO4 EncodedAdt
encodedConstructor i n args = Exception.assert (i < n) 
                            $ withAdtCache (constant True,flags,args)
  where
    flags = case n of 1 -> []
                      _ -> map constant $ invNumeric n i

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

constantConstructorIndex :: Integer -> EncodedAdt -> Maybe Integer
constantConstructorIndex n adt = case flags adt of
  Nothing -> error "EncodedAdt.constantConstructorIndex: no flags"
  Just fs -> primitivesToDecimal n fs

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

constructorArgument :: Integer -> Integer -> Integer -> EncodedAdt -> EncodedAdt
constructorArgument _ _ _ adt | isConstantlyUndefined adt = encUndefined
constructorArgument _ _ _ adt | isEmpty adt               = encEmpty
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

origin :: EncodedAdt -> Doc
origin Empty = text "empty"
origin adt    = _origin adt

-- * Utilities

-- |@onValidDiscriminant d n f@
--  * returns 'Empty', if @d@ is empty or @discriminates n (flags' d) == False@
--  * returns 'encUndefined', if @isConstantlyUndefined d@ is true
--  * returns @f@ otherwise
onValidDiscriminant :: EncodedAdt -> Integer -> CO4 EncodedAdt -> CO4 EncodedAdt
onValidDiscriminant d n f = 
  if isConstantlyUndefined d then return encUndefined
  else if isEmpty d then return encEmpty
       else if discriminates n (flags' d) 
            then f
            else return encEmpty

-- |@isValidDiscriminant d n@ checks if @d@ is a valid discriminant, i.e.
--  * if @d@ is not constantly undefined
--  * if @d@ is not empty
--  * if @discriminates n (flags' d) == True@
isValidDiscriminant :: EncodedAdt -> Integer -> Bool
isValidDiscriminant adt n = Prelude.and [ not $ isConstantlyUndefined adt
                                        , not $ isEmpty               adt
                                        , discriminates n (flags' adt)
                                        ]

-- |@ifReachable d i n b@ evaluates the @i@-th branch @b@ of a case-distinction with @n@
-- constructors on discriminat @d@, if the @i@-th branch is reachable according to @d@, i.e.
-- if @d@ is not constant or if @d@ is constant @i@.
ifReachable :: EncodedAdt -> Integer -> Integer -> CO4 EncodedAdt -> CO4 EncodedAdt
ifReachable d i n b = Exception.assert (isValidDiscriminant d n) $
  case primitivesToDecimal n $ flags' d of
    Nothing         -> b
    Just j | i == j -> b
    Just _          -> return encEmpty

-- |Case distinction between encoded ADTs
caseOf :: EncodedAdt -> [EncodedAdt] -> CO4 EncodedAdt
caseOf adt branches | isConstantlyUndefined adt 
                   || (all isConstantlyUndefined branches) 
                   || (all (\x -> isConstantlyUndefined x || isEmpty x) branches)
                    = return encUndefined
caseOf adt branches | isEmpty adt || (all isEmpty branches)
                    = return Empty
caseOf adt branches | not (discriminates (genericLength branches) (flags' adt))
                    = error "EncodedAdt.caseOf: missing flags (use 'onValidDiscriminant')"
caseOf adt branches =
  case constantConstructorIndex numCons adt of
    Just i  -> Exception.assert (i < genericLength branches) 
             $ return $ branches `genericIndex` i
    Nothing -> do 
      [branchDef] <- caseOfBits fs $ map (Just . return . definedness) branches

      def'        <- and [branchDef, definedness adt]
      id'         <- newId
      origin'     <- mergeOrigins branches

      let adt'    = EncodedAdt id' def' fs (fromJust $ arguments adt) origin'

      flags'      <- caseOfBits fs $ map flags branches
      arguments'  <- caseOfArguments adt' $ map arguments branches

      return $ EncodedAdt id' def' flags' arguments' origin'
  where
    numCons = genericLength branches
    fs      = flags' adt

mergeOrigins :: [EncodedAdt] -> CO4 Doc
mergeOrigins branches =
  isProfileRun >>= \case
    False -> return empty
    True  -> do
      trace <- getCallStackTrace
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

toIntermediateAdt :: (Decode m Primitive Bool) 
                  => EncodedAdt -> Integer -> m IntermediateAdt
toIntermediateAdt adt _ | isConstantlyUndefined adt = return IntermediateUndefined 
toIntermediateAdt Empty _                           = return IntermediateEmpty
toIntermediateAdt (EncodedAdt _ definedness flags args _) n = 
  Exception.assert (length flags >= bitWidth n) $ do
    decode definedness >>= \case 
      False -> return IntermediateUndefined
      True  -> decode flags >>= return . intermediate . numeric n
        where
          intermediate i = IntermediateConstructorIndex i args 

primitivesToDecimal :: Integer -> [Primitive] -> Maybe Integer
primitivesToDecimal n ps = 
  if all P.isConstant ps
  then Just $ numeric n $ map (fromJust . P.evaluateConstant) ps
  else Nothing

caseOfBits :: [Primitive] -> [Maybe [Primitive]] -> CO4 [Primitive]
caseOfBits flags branchBits = 
    Exception.assert (not $ null nonEmptyBits) 
  $ Exception.assert (discriminates numCons flags) 
  $ case all equalBits (transpose branchBits') of
      True  -> return $ head $ branchBits'
      False -> case primitivesToDecimal numCons flags of
        Nothing -> forM (transpose branchBits') mergeN 
        Just i  -> return $ branchBits' `genericIndex` i
    where
      numCons             = genericLength branchBits
      nonEmptyBits        = catMaybes branchBits
      longestNonEmptyBits = maximumBy (compare `on` length) nonEmptyBits
      branchBits'         = for branchBits $ \case
        Nothing -> longestNonEmptyBits
        Just bs -> bs ++ drop (length bs) longestNonEmptyBits

      equalBits bs = all (\b -> b == head bs) bs

      mergeN bitsT = case equalBits bitsT of
        True  -> return $ head bitsT 
        False -> do
           r <- primitive
           forM (zip bitsT [0..]) $ \ (b, i) -> do
                let pattern = invNumeric numCons i
                    fs      = zipWith select pattern flags
                    
                assert ( r : P.not b : map P.not fs  )
                assert ( P.not r :  b : map P.not fs  )
           return r

trimFlags :: Int -> EncodedAdt -> EncodedAdt
trimFlags n adt = case flags adt of
  Nothing -> adt
  Just fs -> adt { _flags = take n fs }
