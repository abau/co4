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
    excludeEmptyAndInvalidConstructorPatterns result alloc
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
                      AllocateEmpty            -> []
                    BuiltIn _ -> []

  flags <- case allocs of
    [Known 0 1 _] -> return []
    [Known i n _] -> return $ map constant $ toBinary (Just $ bitWidth n) i
    [BuiltIn  n ] -> sequence $ replicate n primitive
    _             -> sequence $ replicate (bitWidth maxConstructors) primitive

  make (constant True) flags args

  where 
    maxConstructors = maximum $ for allocs $ \case 
                        Known _ n _  -> n
                        Unknown cons -> toInteger $ length cons 
                        BuiltIn n    -> 2^n

    maxArgs = maximum $ for allocs $ \case
      Known _ _ args -> length args
      Unknown cons   -> maximum $ for cons $ \case
        AllocateConstructor args -> length args
        AllocateEmpty            -> 0
      BuiltIn _ -> 0

excludeEmptyAndInvalidConstructorPatterns :: EncodedAdt -> Allocator -> CO4 ()
excludeEmptyAndInvalidConstructorPatterns = go [] []
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


          AllocateEmpty -> excludePattern (flags   ++ thisFlags)
                                          (pattern ++ thisPattern)

          where
            thisPattern = case thisFlags of
              [] -> []
              _  -> toBinary (Just $ length thisFlags) i

    go _ _ _ (BuiltIn _) = return ()

excludePattern :: [Primitive] -> [Bool] -> CO4 ()
excludePattern []    []      = return ()
excludePattern flags pattern = Exception.assert (length flags == length pattern)
                             $ assert $ zipWith antiSelect pattern flags 
