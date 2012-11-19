{-# language MultiParamTypeClasses #-}
module CO4.EncodedAdt
  ( EncodedAdt (..), EncodedArguments (..), IntermediateAdt (..)
  , toIntermediateAdt, encUnit, encUndefined, encDontCare, encArgs
  , encDontCareArgs
  , constructorArgument, encodedConsCall, unknownConstructor
  , switchBy
  )
where

import           Prelude hiding (not,or,and)
import           Control.Applicative ((<$>))
import           Control.Monad (ap,zipWithM)
import qualified Control.Exception as Exception 
import           Data.Maybe (isNothing)
import           Data.Tree
import           Satchmo.SAT.Mini (SAT)
import           Satchmo.Code 
import           Satchmo.Boolean 

data EncodedAdt =
    EncSingle EncodedArguments
  | EncEither { flag        :: Boolean    
              , left        :: EncodedArguments
              , right       :: Either EncodedArguments EncodedAdt
              }                      
  | EncUndefined 
  | EncDontCare

data EncodedArguments = EncArgs { fromEncArgs :: [EncodedAdt] } 
                      | EncDontCareArgs
                      | EncUndefinedArgs

toTree :: EncodedAdt -> Tree String
toTree encodedAdt = case encodedAdt of
  EncSingle args -> Node "single" $ argsToTrees args
  EncEither _ l (Left r) ->
    Node "either" [Node "1" $ argsToTrees l, Node "2" $ argsToTrees r]

  EncEither _ l (Right r) ->
    let Node _ rs = toTree r
    in
      Node "either" $ rs ++ [Node (show $ length rs + 1) $ argsToTrees l]

  EncUndefined -> Node "undefined" []
  EncDontCare  -> Node "dont-care" []

  where argsToTrees (EncArgs adts)   = map toTree adts
        argsToTrees EncDontCareArgs  = [Node "dont-care-arguments" []]
        argsToTrees EncUndefinedArgs = [Node "undefined-arguments" []]

instance Show EncodedAdt where
  show = drawTree . toTree

-- |The construction of an intermediate ADT simplifies the derivation of the
-- actual @Decode@ instance.
data IntermediateAdt = IntermediateConstructorIndex Int [EncodedAdt]
                     | IntermediateUndefined

toIntermediateAdt :: EncodedAdt -> SAT IntermediateAdt 
toIntermediateAdt = go 0
  where 
    go _ EncUndefined     = return IntermediateUndefined
    go _ (EncSingle args) = return $ IntermediateConstructorIndex 0 $ fromEncArgs args

    go depth (EncEither flag left right) = do
      flag' <- decode flag
      case (flag',right) of
        (True,_)          -> return $ IntermediateConstructorIndex depth       
                                    $ fromEncArgs left
        (False,Left args) -> return $ IntermediateConstructorIndex (depth + 1) 
                                    $ fromEncArgs args
        (False,Right adt) -> go (depth + 1) adt

encUndefined :: EncodedAdt
encUndefined = EncUndefined

encDontCare :: EncodedAdt
encDontCare = EncDontCare

encUnit :: EncodedArguments
encUnit = EncArgs []

encArgs :: [EncodedAdt] -> EncodedArguments
encArgs = EncArgs

encDontCareArgs ::  EncodedArguments
encDontCareArgs = EncDontCareArgs

-- |@constructorArgument i j a@ retrieves the @i@-th argument of the @j@-th
-- constructor of @a@.
constructorArgument :: Int -> Int -> EncodedAdt -> EncodedAdt
constructorArgument i = go 0
  where
    go 0 0 (EncSingle args) = fromArgs args

    go depth consIndex (EncEither _ left right) = 
      if depth == consIndex 
      then fromArgs left
      else case right of
        Left args | depth == (consIndex - 1) -> fromArgs args
        Right adt                            -> go (depth + 1) consIndex adt

    fromArgs (EncArgs args)  = args !! i
    fromArgs EncDontCareArgs = EncDontCare
    fromArgs EncUndefinedArgs = EncUndefined

-- |@encodedConsCall i args@ encodes the call of the @i@-th constructor of an ADT,
-- where @args@ is a list of the encoded arguments of all constructors of the ADT,
-- i.e. @i < length args@.
encodedConsCall :: Int -> [EncodedArguments] -> EncodedAdt
encodedConsCall = go 0
  where
    go 0 0 [a] = EncSingle a

    go depth consIndex [a,b] = 
      if depth == consIndex 
      then EncEither (Constant True)  a $ Left b
      else EncEither (Constant False) a $ Left b

    go depth consIndex (x:xs) = 
      let adt = go (depth+1) consIndex xs
      in
        if depth == consIndex 
        then EncEither (Constant True)  x $ Right adt
        else EncEither (Constant False) x $ Right adt

-- |@unknownConstructor args@ encodes the call to an unknown constructor,
-- where @args@ is a list of the encoded arguments of all constructors of the ADT.
unknownConstructor :: [Maybe EncodedArguments] -> SAT EncodedAdt
unknownConstructor arguments = case arguments of
  [Just a]         -> return $ EncSingle a
  [Just a,Just b]  -> return   EncEither `ap` boolean 
                                         `ap` return a 
                                         `ap` return (Left b)
  [Just a,Nothing] -> return $ EncEither (Constant True)  a $ Left EncUndefinedArgs
  [Nothing,Just b] -> return $ EncEither (Constant False) EncUndefinedArgs $ Left b

  (Just x:xs) | all isNothing xs -> 
    return $ EncEither (Constant True) x $ Right EncUndefined

  (Just x:xs) -> do
    flag  <- boolean
    EncEither flag x . Right <$> unknownConstructor xs
  (Nothing:xs)    -> 
    EncEither (Constant False) EncUndefinedArgs . Right <$> unknownConstructor xs

-- |Builds a Dont-Care instance from an ADT
dontCare :: EncodedAdt -> SAT EncodedAdt
dontCare encodedAdt = case encodedAdt of
  EncSingle _             -> return $ EncSingle $ EncDontCareArgs 
  EncEither _ _ (Left _)  -> do flag <- boolean
                                return $ EncEither flag EncDontCareArgs $ Left EncDontCareArgs
  EncEither _ _ (Right _) -> do flag <- boolean
                                return $ EncEither flag EncDontCareArgs $ Right EncDontCare
  EncDontCare             -> return EncDontCare

-- |Class for selecting a value by a boolean flag
class Select a where
  select :: Boolean -> a -> SAT a

instance (Select a, Select b) => Select (Either a b) where
  select flag (Left  a) = Left  <$> select flag a
  select flag (Right b) = Right <$> select flag b

instance Select Boolean where
  select flag a = and [flag,a]

instance Select EncodedAdt where
  select flag (EncEither f l r) = do
    f' <- select flag f
    l' <- select flag l
    r' <- select flag r
    return $ EncEither f' l' r'

  select flag (EncSingle a) = EncSingle <$> select flag a

  select _ EncDontCare      = return EncDontCare

instance Select EncodedArguments where
  select flag (EncArgs as)  = EncArgs <$> mapM (select flag) as
  select _ EncDontCareArgs  = return EncDontCareArgs
  select _ EncUndefinedArgs = return EncUndefinedArgs

-- |Class for choosing one of two alternatives by a boolean flag
class Choose a where
  choose :: Boolean -> a -> a -> SAT a

instance (Choose a, Choose b) => Choose (Either a b) where
  choose flag (Left  a1) (Left  a2) = Left  <$> choose flag a1 a2
  choose flag (Right b1) (Right b2) = Right <$> choose flag b1 b2

instance Choose Boolean where
  choose flag a b = do
    a' <- flag     `select` a
    b' <- not flag `select` b
    or [ a', b' ]

instance Choose EncodedAdt where
  choose flag (EncSingle a1) (EncSingle a2) = EncSingle <$> choose flag a1 a2

  choose flag (EncEither f1 l1 r1) (EncEither f2 l2 r2) = do
    f <- choose flag f1 f2
    l <- choose flag l1 l2
    r <- choose flag r1 r2
    return $ EncEither f l r

  --choose flag EncDontCare EncUndefined  = return EncDontCare
  --choose flag EncUndefined EncDontCare  = return EncDontCare

  choose _ EncUndefined EncUndefined = return EncUndefined
  choose flag adt EncUndefined       = assert [flag]     >> flag     `select` adt
  choose flag EncUndefined adt       = assert [not flag] >> not flag `select` adt

  choose _ EncDontCare EncDontCare   = return EncDontCare

  choose flag adt EncDontCare = dontCare adt >>= choose flag adt 
  choose flag EncDontCare adt = dontCare adt >>= flip (choose flag) adt 


instance Choose EncodedArguments where
  choose flag (EncArgs as) (EncArgs bs) = 
    Exception.assert (length as == length bs) $
      EncArgs <$> zipWithM (choose flag) as bs

  choose flag (EncArgs as) EncDontCareArgs = 
    EncArgs <$> zipWithM (choose flag) as (repeat EncDontCare)

  choose flag EncDontCareArgs (EncArgs as) = 
    EncArgs <$> zipWithM (choose flag) (repeat EncDontCare) as 

  choose _ _ EncDontCareArgs = return EncDontCareArgs 
  choose _ EncDontCareArgs _ = return EncDontCareArgs 

  choose _ _ EncUndefinedArgs = return EncUndefinedArgs
  choose _ EncUndefinedArgs _ = return EncUndefinedArgs

switch :: Choose a => [Boolean] -> [a] -> SAT a
switch _      [a]    = return a
switch (f:_) [a,b]   = choose f a b
switch (f:fs) (x:xs) = switch fs xs >>= choose f x  

switchBy :: EncodedAdt -> [EncodedAdt] -> SAT EncodedAdt
switchBy adt = switch (flags adt)
  where flags encodedAdt = case encodedAdt of
          EncEither f _ (Left _)  -> [f]
          EncEither f _ (Right r) -> f : flags r
