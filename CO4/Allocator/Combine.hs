{-# LANGUAGE LambdaCase #-}
module CO4.Allocator.Combine
  ()
where

import qualified Control.Exception as Exception
import           Control.Monad (forM_)
import           Data.List (transpose)
import           Satchmo.Core.MonadSAT (MonadSAT)
import           Satchmo.Core.Primitive (Primitive,primitive,antiSelect,assert)
import           CO4.Allocator.Common (Allocator (..),AllocateConstructor (..))
import           CO4.Encodeable (Encodeable (..))
import           CO4.EncodedAdt.Sequential (EncodedAdt (..), EncodedConstructor)
import qualified CO4.EncodedAdt.Sequential as E
import           CO4.Util (for,bitWidth,binaries,toBinary)

instance Encodeable Allocator where
  encode allocator = do
    [adt] <- encodeCombined [ Just allocator ]
    excludeBottomAndInvalidConstructorPatterns adt
    return adt

  encodeConstant = \case
    Known i n as -> E.encodedConstructor i n $ map encodeConstant as

encodeCombined :: (MonadSAT m, Primitive p) => [Maybe Allocator] -> m [EncodedAdt p]
encodeCombined [] = return []
encodeCombined allocs = do
  sharedFlags  <- sequence $ replicate maxNumUnknownFlags primitive
  constructors <- encodeCombinedConstructors $ concat $ for allocs $ \case
                      Nothing               -> []
                      Just (Known _ _ args) -> [AllocateConstructor args]
                      Just (Unknown cons)   -> cons
  
  return $ makeAdts sharedFlags constructors allocs

  where
    maxNumUnknownFlags = maximum $ for allocs $ \case
        Nothing             -> 0
        Just (Known {})     -> 0
        Just (Unknown cons) -> bitWidth $ length cons

    makeAdts sharedFlags constructors allocs = case (constructors, allocs) of
      ([],[]) -> []
      (_ ,[]) -> error "Allocator.Combine.encodeCombined: orphaned constructors"
      (_ , Nothing : as) -> E.bottom : makeAdts sharedFlags constructors as

      (c:cs, (Just (Known i n _)) : as) -> (E.encodedConstructor i n c) 
                                         : (makeAdts sharedFlags cs as)

      (c:cs, (Just (Unknown [_])) : as) -> 
        (E.encodedConstructor 0 1 c) : (makeAdts sharedFlags cs as)

      (cs  , (Just (Unknown allocCons)) : as) -> 
        let n           = length allocCons
            flags       = Exception.assert (bitWidth n <= length sharedFlags)
                        $ take (bitWidth n) sharedFlags
            (cons, cs') = splitAt n cs
        in
          (EncodedAdt flags cons) : (makeAdts sharedFlags cs' as)

encodeCombinedConstructors :: (MonadSAT m, Primitive p) 
                           => [AllocateConstructor] -> m [EncodedConstructor p]
encodeCombinedConstructors allocCons = do
  argssT <- mapM encodeCombined $ transpose $ map pad allocCons 

  let argss = transpose argssT
      mkEncodedConstructor _     AllocateBottom             = E.bottomConstructor
      mkEncodedConstructor args (AllocateConstructor args') = take (length args') args

      encodedConstructors = case argss of
        [] -> zipWith mkEncodedConstructor (repeat undefined) allocCons
        _  -> zipWith mkEncodedConstructor argss allocCons

  return encodedConstructors
  where
    maxNumArguments = maximum $ for allocCons $ \case
      AllocateConstructor args -> length args
      AllocateBottom           -> 0

    pad AllocateBottom             = replicate maxNumArguments Nothing
    pad (AllocateConstructor args) = 
      map Just args ++ (replicate (maxNumArguments - (length args)) Nothing)

excludeBottomAndInvalidConstructorPatterns :: (MonadSAT m, Primitive p) 
                                           => EncodedAdt p -> m ()
excludeBottomAndInvalidConstructorPatterns = go [] []
  where
    go flags pattern adt | E.isBottom adt = excludePattern flags pattern

    go flags pattern adt@(EncodedAdt _ conss) | E.constantConstructor adt = 
      mapM_ (mapM_ $ go flags pattern) conss

    go flags pattern (EncodedAdt [] [args]) = forM_ args $ go flags pattern

    go flags pattern (EncodedAdt fs conss) = do
      forM_ invalidConstructorPatterns $ \p ->
        excludePattern (flags ++ fs) (pattern ++ p) 

      forM_ (zip [0..] conss) $ \(i,cons) ->
        forM_ cons $ go (flags ++ fs) (pattern ++ (constructorPattern i))
      where
        invalidConstructorPatterns = drop (length conss) $ binaries $ length fs

        constructorPattern = toBinary $ Just $ length fs

excludePattern :: (MonadSAT m, Primitive p) => [p] -> [Bool] -> m ()
excludePattern [] [] = return ()
excludePattern flags pattern = Exception.assert (length flags == length pattern)
                             $ assert $ zipWith antiSelect pattern flags 

