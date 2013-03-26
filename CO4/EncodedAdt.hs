{-# language FlexibleContexts #-}
module CO4.EncodedAdt 
  ( EncodedAdt, IntermediateAdt (..)
  , flags, encodedUndefined, isUndefined, encode, caseOf, encodedConsCall
  , constructorArgument, toIntermediateAdt
  )
where

import           Control.Monad (liftM)
import qualified Control.Exception as Exception 
import           Data.Tree
import           Satchmo.Core.MonadSAT (MonadSAT)
import           Satchmo.Core.Decode (Decode,decode)
import           Satchmo.Core.Primitive (Primitive)
import qualified CO4.UnknownAdt as U
import qualified CO4.Allocator as A
import           CO4.Util (fromBinary)

data EncodedAdt p = EncodedConstructor { constructorIndex  :: Int
                                       , _numConstructors  :: Int
                                       , _arguments        :: [EncodedAdt p]
                                       }
                  | EncodedUnknown (U.UnknownAdt p)

instance Show p => Show (EncodedAdt p) where
  show = drawTree . toTree 
    where
      toTree = \case
        EncodedConstructor i n args -> Node (concat ["cons ",show i," of ",show n]) 
                                     $ map toTree args
        EncodedUnknown unknownAdt   -> U.toTree unknownAdt

flags :: EncodedAdt p -> Maybe [p]
flags = \case 
  EncodedConstructor {} -> Nothing
  EncodedUnknown     u  -> Just $ U.flags u

encodedUndefined :: EncodedAdt p
encodedUndefined = EncodedUnknown U.UUndefined

isUndefined :: EncodedAdt p -> Bool
isUndefined = \case EncodedUnknown U.UUndefined -> True
                    _                           -> False

encode :: (MonadSAT m, Primitive p) => A.Allocator -> m (EncodedAdt p)
encode = \case 
  A.Known i n as -> liftM (EncodedConstructor i n) $ mapM encode as
  A.Unknown u    -> 
    case u of
      A.AllocateUnknown [A.AllocateConstructor args] -> 
        liftM (EncodedConstructor 0 1) $ mapM (encode . A.Unknown) args
      _ -> liftM EncodedUnknown $ U.unknown u

caseOf :: (MonadSAT m, Primitive p) => EncodedAdt p -> [EncodedAdt p] -> m (EncodedAdt p)
caseOf adt branches = case adt of
  EncodedConstructor {} -> return $ branches !! (constructorIndex adt)
  EncodedUnknown     u  -> liftM EncodedUnknown $ U.caseOf u $ map toUnknown branches
    where
      toUnknown = \case 
        EncodedUnknown u          -> u
        EncodedConstructor i n as -> U.encodedConsCall i n $ map toUnknown as

encodedConsCall :: Primitive p => Int -> Int -> [EncodedAdt p] -> EncodedAdt p
encodedConsCall i n = Exception.assert (i < n) $ EncodedConstructor i n

constructorArgument :: Int -> Int -> EncodedAdt p -> EncodedAdt p
constructorArgument i j = \case 
  EncodedConstructor j' _ args | j == j' -> args !! i
  EncodedConstructor _  _ _              -> encodedUndefined
  EncodedUnknown u                       -> EncodedUnknown 
                                          $ U.constructorArgument i j u

data IntermediateAdt p = IntermediateConstructorIndex Int [EncodedAdt p]
                       | IntermediateUndefined

toIntermediateAdt :: (MonadSAT m, Primitive p, Decode m p Bool) 
             => EncodedAdt p -> m (IntermediateAdt p)
toIntermediateAdt = \case 
  EncodedConstructor i _ as   -> return $ IntermediateConstructorIndex i as
  EncodedUnknown U.UUndefined -> return IntermediateUndefined 
  EncodedUnknown adt          -> 
    if null (U.flags adt) 
    then return $ intermediate 0
    else decode (U.flags adt) >>= return . intermediate . fromBinary
    where
      intermediate i = case U.constructorArguments i adt of
        Nothing -> IntermediateUndefined
        Just as -> IntermediateConstructorIndex i $ map EncodedUnknown as
