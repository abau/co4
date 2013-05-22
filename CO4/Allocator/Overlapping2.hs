{-# LANGUAGE LambdaCase #-}
module CO4.Allocator.Overlapping2
  ()
where

import qualified Control.Exception as Exception
import           Control.Monad (liftM,forM_,zipWithM_)
import           Data.Maybe (isNothing,catMaybes)
import           Data.List ((\\),transpose)
import           Satchmo.Core.MonadSAT (MonadSAT)
import           Satchmo.Core.Primitive (Primitive,primitive,constant,antiSelect,assert)
import           CO4.Allocator.Common (Allocator (..),AllocateConstructor (..))
import           CO4.Encodeable (Encodeable (..))
import           CO4.EncodedAdt.Overlapping (EncodedAdt (..))
import qualified CO4.EncodedAdt.Overlapping as E
import qualified CO4.EncodedAdt.Overlapping2 as E2
import           CO4.Util (for,bitWidth,binaries,toBinary)

instance Encodeable Allocator where
  encode alloc = do
    result <- encodeOverlapping [alloc]
    excludeBottomAndInvalidConstructorPatterns result alloc
    return $ E2.EncodedAdt (Just alloc) result

  encodeConstant alloc = E2.EncodedAdt (Just alloc) $ go alloc
    where go (Known i n as) = E.encodedConstructor i n $ map go as

encodeOverlapping :: (MonadSAT m, Primitive p) => [Allocator] -> m (EncodedAdt p)
encodeOverlapping []     = error "Allocator.Overlapping.encodeOverlapping: no allocators"
encodeOverlapping allocs = do
  args <- case maxArgs of
            0 -> return []
            _ -> mapM encodeOverlapping $ transpose $ concat $ for allocs $ \case 
                    Known _ _ args -> [ args ]
                    Unknown cons   -> for cons $ \case 
                      AllocateConstructor args -> args
                      AllocateBottom           -> []

  flags <- case allocs of
    [Known 0 1 _] -> return []
    [Known i n _] -> return $ map constant $ toBinary (Just $ bitWidth n) i
    _             -> sequence $ replicate (bitWidth maxConstructors) primitive

  return $ EncodedAdt flags args

  where 
    maxConstructors = maximum $ for allocs $ \case 
                        Known _ n _  -> n
                        Unknown cons -> length cons 

    maxArgs = maximum $ for allocs $ \case
      Known _ _ args -> length args
      Unknown cons   -> maximum $ for cons $ \case
        AllocateConstructor args -> length args
        AllocateBottom           -> 0

excludeBottomAndInvalidConstructorPatterns :: (MonadSAT m, Primitive p) 
                                           => EncodedAdt p -> Allocator -> m ()
excludeBottomAndInvalidConstructorPatterns = go [] []
  where
    go flags pattern (EncodedAdt [] args) (Known 0 1 args') =
        Exception.assert (length args >= length args') 
      $ zipWithM_ (go flags pattern) args args'

    go flags pattern (EncodedAdt fs args) (Known i n args') =
        Exception.assert (length args >= length args') 
      $ Exception.assert (bitWidth n <= length fs)
      $ 
        let thisFlags       = take (bitWidth n) fs
            thisPattern     = toBinary (Just $ length thisFlags) i
            invalidPatterns = binaries (length thisFlags) \\ [thisPattern]
        in do 
          forM_ invalidPatterns $ \p -> excludePattern (flags   ++ thisFlags) 
                                                       (pattern ++ p)
          zipWithM_ (go (flags ++ thisFlags) (pattern ++ thisPattern)) args args'
          
    go flags pattern (EncodedAdt fs args) (Unknown cons) =
        Exception.assert (length fs >= bitWidth (length cons))
      $ do
          forM_ invalidPatterns $ \p -> excludePattern (flags   ++ thisFlags)
                                                       (pattern ++ p)
          zipWithM_ goCons [0..] cons

      where 
        thisFlags       = take (bitWidth $ length cons) fs
        invalidPatterns = drop (length cons) $ binaries $ length thisFlags

        goCons i = \case
          AllocateConstructor allocs -> Exception.assert (length allocs <= length args) 
            $ zipWithM_ (go (flags ++ thisFlags) (pattern ++ thisPattern)) args allocs


          AllocateBottom -> excludePattern (flags   ++ thisFlags)
                                           (pattern ++ thisPattern)

          where
            thisPattern = case thisFlags of
              [] -> []
              _  -> toBinary (Just $ length thisFlags) i

    go flags pattern _ _ = error $ "Allocator.Overlapping.excludeBottomAndInvalidConstructorPatterns: bottom"

excludePattern :: (MonadSAT m, Primitive p) => [p] -> [Bool] -> m ()
excludePattern []    []      = return ()
excludePattern flags pattern = Exception.assert (length flags == length pattern)
                             $ assert $ zipWith antiSelect pattern flags 
