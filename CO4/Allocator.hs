{-# LANGUAGE LambdaCase #-}
module CO4.Allocator
  (module CO4.AllocatorData)
where

import qualified Control.Exception as Exception
import           Control.Monad (forM_,zipWithM_)
import           Data.List ((\\),transpose)
import           Satchmo.Core.Primitive (primitive,constant,antiSelect,assert)
import           CO4.AllocatorData 
import           CO4.Encodeable (Encodeable (..))
import           CO4.EncodedAdt (Primitive,EncodedAdt,flags',arguments',make)
import           CO4.Util (for,bitWidth,binaries,toBinary)
import           CO4.Monad (CO4)

instance Encodeable Allocator where
  encode alloc = do
    result <- encodeOverlapping [alloc]
    excludeBottomAndInvalidConstructorPatterns result alloc
    return result

encodeOverlapping :: [Allocator] -> CO4 EncodedAdt
encodeOverlapping []     = error "Allocator.encodeOverlapping: no allocators"
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

  make (constant True) flags args

  where 
    maxConstructors = maximum $ for allocs $ \case 
                        Known _ n _  -> n
                        Unknown cons -> length cons 

    maxArgs = maximum $ for allocs $ \case
      Known _ _ args -> length args
      Unknown cons   -> maximum $ for cons $ \case
        AllocateConstructor args -> length args
        AllocateBottom           -> 0

excludeBottomAndInvalidConstructorPatterns :: EncodedAdt -> Allocator -> CO4 ()
excludeBottomAndInvalidConstructorPatterns = go [] []
  where
    go flags pattern adt (Known 0 1 args') =
        Exception.assert (length args >= length args') 
      $ Exception.assert (null $ flags' adt)
      $ zipWithM_ (go flags pattern) args args'
      where
        args = arguments' adt

    go flags pattern adt (Known i n args') =
        Exception.assert (length args >= length args') 
      $ Exception.assert (bitWidth n <= length fs)
      $ do 
          forM_ invalidPatterns $ \p -> excludePattern (flags   ++ thisFlags) 
                                                       (pattern ++ p)
          zipWithM_ (go (flags ++ thisFlags) (pattern ++ thisPattern)) args args'
        where
          args            = arguments' adt
          fs              = flags' adt
          thisFlags       = take (bitWidth n) fs
          thisPattern     = toBinary (Just $ length thisFlags) i
          invalidPatterns = binaries (length thisFlags) \\ [thisPattern]
          
    go flags pattern adt (Unknown cons) =
        Exception.assert (length fs >= bitWidth (length cons))
      $ do
          forM_ invalidPatterns $ \p -> excludePattern (flags   ++ thisFlags)
                                                       (pattern ++ p)
          zipWithM_ goCons [0..] cons
      where 
        args            = arguments' adt
        fs              = flags' adt
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

excludePattern :: [Primitive] -> [Bool] -> CO4 ()
excludePattern []    []      = return ()
excludePattern flags pattern = Exception.assert (length flags == length pattern)
                             $ assert $ zipWith antiSelect pattern flags 
