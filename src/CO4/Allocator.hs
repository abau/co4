{-# LANGUAGE LambdaCase #-}
module CO4.Allocator
  (module CO4.Allocator.Data, module CO4.Allocator.Typed)
where

import qualified Control.Exception as Exception
import           Control.Monad (zipWithM_)
import           Data.List (transpose,genericLength)
import           Satchmo.Core.Primitive (primitive,constant,antiSelect,select,assert)
import qualified Satchmo.Core.Primitive as P
import           CO4.Allocator.Data 
import           CO4.Allocator.Typed
import           CO4.Encodeable (Encodeable (..))
import           CO4.EncodedAdt (Primitive,EncodedAdt,flags',arguments',make)
import           CO4.Util (for,bitWidth)
import           CO4.Monad (CO4)
import           CO4.Prefixfree (invNumeric)

instance Encodeable (TAllocator t) where
  encode = encode . toAllocator

instance Encodeable Allocator where
  encode alloc = do
    result <- encodeOverlapping [alloc]
    postprocessFlags result alloc
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
                    BuiltInKnown _   -> []
                    BuiltInUnknown _ -> []

  flags <- case allocs of
    [Known 0 1 _]     -> return []
    [Known i n _]     -> return $ map constant $ invNumeric n i
    [BuiltInKnown fs] -> return $ map constant fs
    _                 -> sequence $ replicate maxFlags primitive

  make (constant True) flags args prefixfree

  where 
    maxFlags = maximum $ for allocs $ \case 
                 Known _ n _      -> bitWidth n
                 Unknown cons     -> bitWidth $ genericLength cons 
                 BuiltInKnown  fs -> length fs
                 BuiltInUnknown n -> n

    maxArgs = maximum $ for allocs $ \case
      Known _ _ args -> length args
      Unknown cons   -> maximum $ for cons $ \case
        AllocateConstructor args -> length args
        AllocateEmpty            -> 0
      BuiltInKnown _   -> 0
      BuiltInUnknown _ -> 0

    prefixfree = and $ for allocs $ \case
      Known          {} -> True
      Unknown        {} -> True
      BuiltInKnown   {} -> False
      BuiltInUnknown {} -> False

-- 1. excludes patterns that lead to Empty (end of recursions)
-- 2. implies constant flags from parental patterns
postprocessFlags :: EncodedAdt -> Allocator -> CO4 ()
postprocessFlags = go [] []
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
      $ do (flags,pattern) `implyPattern` (thisFlags, thisPattern)
           zipWithM_ (go (flags ++ thisFlags) (pattern ++ thisPattern)) args args'
        where
          args            = arguments' adt
          fs              = flags' adt
          thisPattern     = invNumeric n i
          thisFlags       = take (length thisPattern) fs
          
    go flags pattern adt (Unknown cons) =
        Exception.assert (length fs >= bitWidth (length cons))
      $ zipWithM_ goCons [0..] cons
      where 
        args            = arguments' adt
        fs              = flags' adt

        goCons i = \case
          AllocateConstructor allocs -> Exception.assert (length allocs <= length args) 
            $ zipWithM_ (go (flags ++ thisFlags) (pattern ++ thisPattern)) args allocs


          AllocateEmpty -> excludePattern (flags   ++ thisFlags)
                                          (pattern ++ thisPattern)

          where
            thisPattern = invNumeric (fromIntegral $ length cons) i
            thisFlags   = take (length thisPattern) fs

    go _ _ _ (BuiltInKnown   _) = return ()
    go _ _ _ (BuiltInUnknown _) = return ()

excludePattern :: [Primitive] -> [Bool] -> CO4 ()
excludePattern []    []      = return ()
excludePattern flags pattern = Exception.assert (length flags == length pattern)
                             $ assert $ zipWith antiSelect pattern flags 

implyPattern :: ([Primitive],[Bool]) -> ([Primitive],[Bool]) -> CO4 ()
implyPattern premise conclusion = do
  p <- P.and $ zipWith select (snd premise)    (fst premise)
  c <- P.and $ zipWith select (snd conclusion) (fst conclusion)
  r <- P.implies p c
  assert [r]
