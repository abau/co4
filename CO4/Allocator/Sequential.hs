{-# LANGUAGE LambdaCase #-}
module CO4.Allocator.Sequential
  ()
where

import qualified Control.Exception as Exception
import           Control.Monad (liftM,forM_)
import           Satchmo.Core.MonadSAT (MonadSAT)
import           Satchmo.Core.Primitive (Primitive,primitive,antiSelect,assert)
import           CO4.Allocator.Common (Allocator (..),AllocateConstructor (..))
import           CO4.Encodeable (Encodeable (..))
import           CO4.EncodedAdt.Sequential (EncodedAdt (..), EncodedConstructor)
import qualified CO4.EncodedAdt.Sequential as E
import           CO4.Util (bitWidth,binaries,toBinary)

instance Encodeable Allocator where
  encode = \case
    Known i n as -> liftM (E.encodedConstructor i n) $ mapM encode as

    Unknown [allocCons] -> do
      cons <- encodeConstructor allocCons 
      if E.isBottomConstructor cons
        then error $ "Allocator.encode: single constructor ADT must not have bottom constructor"
        else return $ E.encodedConstructor 0 1 cons

    Unknown allocConss -> do
      flags <- sequence $ replicate (bitWidth $ length allocConss) primitive
      cons  <- mapM encodeConstructor allocConss
      let adt = EncodedAdt flags cons
      excludeBottom adt
      excludeInvalidConstructorPatterns adt
      return adt
    where
      encodeConstructor :: (MonadSAT m,Primitive p) => AllocateConstructor
                                                    -> m (EncodedConstructor p)
      encodeConstructor AllocateBottom             = return E.bottomConstructor
      encodeConstructor (AllocateConstructor args) = mapM encode args

  encodeConstant = \case
    Known i n as -> E.encodedConstructor i n $ map encodeConstant as

excludeBottom :: (MonadSAT m, Primitive p) => EncodedAdt p -> m ()
excludeBottom = go 
  where
    go (EncodedAdt flags conss) = forM_ (zip [0..] conss) $ uncurry 
                                                          $ goConstructor flags 

    goConstructor flags i cons | E.isBottomConstructor cons = 
      let pattern = toBinary (Just $ length flags) i
      in
        excludePattern flags pattern

    goConstructor _ _ args = forM_ args go 

excludeInvalidConstructorPatterns :: (MonadSAT m, Primitive p) => EncodedAdt p -> m ()
excludeInvalidConstructorPatterns = go
  where
    go (EncodedAdt flags conss) = do
      forM_ nonConstructorPatterns $ excludePattern flags 
      forM_ conss goConstructor 

      where
        nonConstructorPatterns = drop (length conss) $ binaries $ length flags 

    goConstructor cons | E.isBottomConstructor cons = return ()
    goConstructor args = forM_ args go

excludePattern :: (MonadSAT m, Primitive p) => [p] -> [Bool] -> m ()
excludePattern flags pattern = Exception.assert (length flags == length pattern)
                             $ assert $ zipWith antiSelect pattern flags 
