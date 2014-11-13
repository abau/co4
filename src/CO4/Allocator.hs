{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module CO4.Allocator
  (module CO4.Allocator.Data, module CO4.Allocator.Typed)
where

import           Control.Applicative (Applicative)
import qualified Control.Exception as Exception
import           Control.Monad.State.Strict
import           Data.List (transpose,genericLength)
import qualified Data.Map as M
import           Satchmo.Core.Primitive (primitive,constant,antiSelect,select,assert)
import qualified Satchmo.Core.Primitive as P
import           Satchmo.Core.MonadSAT (note)
import           CO4.Allocator.Data hiding (allocatorId)
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

data EncoderData = EncoderData
  { references :: M.Map Int EncodedAdt
  , numShared  :: Int
  }

newtype Encoder a = Encoder { unEnc :: StateT EncoderData CO4 a }
  deriving (Functor, Applicative, Monad, MonadState EncoderData)

liftCO4 :: CO4 a -> Encoder a
liftCO4 = Encoder . lift

incNumShared :: Encoder ()
incNumShared = modify' $ \d -> d { numShared = 1 + (numShared d) }

getReferenced :: Int -> Encoder (Maybe EncodedAdt)
getReferenced id = gets (M.lookup id . references) >>= \case
  Nothing  -> return Nothing
  Just adt -> incNumShared >> return (Just adt)

addReference :: Int -> EncodedAdt -> Encoder ()
addReference id adt = modify' $ \d -> d { references = M.insert id adt $ references d }

encodeOverlapping :: [Allocator] -> CO4 EncodedAdt
encodeOverlapping allocators = do
  (adt, d) <- runStateT (unEnc $ go allocators) $ EncoderData M.empty 0
  note $ unwords ["Number of shared values:", show $ numShared d]
  return adt
  where
    go []     = error "Allocator.encodeOverlapping.go: no allocators"

    go [AllocatorId id alloc] = getReferenced id >>= \case
      Just adt -> return adt
      Nothing  -> do adt <- go [alloc]
                     addReference id adt
                     return adt

    go allocs = do
      args <- case maxArgs of
        0 -> return []
        _ -> mapM go overlappingArgs

      flags <- case allocs of
        [Known 0 1 _]     -> return []
        [Known i n _]     -> return $ map constant $ invNumeric n i
        [BuiltInKnown fs] -> return $ map constant fs
        _                 -> liftCO4 $ sequence $ replicate maxFlags primitive

      liftCO4 $ make (constant True) flags args prefixfree

      where
        overlappingArgs = transpose $ concat $ for allocs go
          where 
            go (Known _ _ args) = [ args ]
            go (Unknown cons)   = for cons $ \case 
                  AllocateConstructor args -> args
                  AllocateEmpty            -> []
            go (BuiltInKnown {})   = []
            go (BuiltInUnknown {}) = []
            go (AllocatorId _ a)   = go a

        maxFlags = maximum $ for allocs go
          where
            go (Known _ n _)      = bitWidth n
            go (Unknown cons)     = bitWidth $ genericLength cons 
            go (BuiltInKnown  fs) = length fs
            go (BuiltInUnknown n) = n
            go (AllocatorId _ a)  = go a

        maxArgs = maximum $ for allocs go
          where
            go (Known _ _ args) = length args
            go (Unknown cons)   = maximum $ for cons $ \case
                  AllocateConstructor args -> length args
                  AllocateEmpty            -> 0
            go (BuiltInKnown {})   = 0
            go (BuiltInUnknown {}) = 0
            go (AllocatorId _ a)   = go a

        prefixfree = and $ for allocs go 
          where
            go (Known          {}) = True
            go (Unknown        {}) = True
            go (BuiltInKnown   {}) = False
            go (BuiltInUnknown {}) = False
            go (AllocatorId   _ a) = go a

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

    go _ _ _ (BuiltInKnown   {}) = return ()
    go _ _ _ (BuiltInUnknown {}) = return ()

    go flags pattern adt (AllocatorId _ a) = go flags pattern adt a

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
