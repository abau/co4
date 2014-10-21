{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module CO4.Allocator
  (module CO4.Allocator.Data, module CO4.Allocator.Typed)
where

import qualified Control.Exception as Exception
import           Control.Monad (zipWithM_,ap)
import           Data.List (transpose,genericLength)
import           Satchmo.Core.Primitive (primitive,constant,antiSelect,select,assert)
import qualified Satchmo.Core.Primitive as P
import           CO4.Allocator.Data 
import           CO4.Allocator.Typed
import           CO4.Encodeable (Encodeable (..))
import           CO4.EncodedAdt (Primitive,EncodedAdt,flags',arguments',make)
import           CO4.Util (for,bitWidth,mapAccumM)
import           CO4.Monad (CO4)
import           CO4.Prefixfree (invNumeric)

instance Encodeable (TAllocator t) where
  encode = encode . toAllocator

instance Encodeable Allocator where
  encode alloc = do
    result <- encodeOverlapping [alloc]
    postprocessFlags result alloc
    return result

data ReferenceableFlags = ReferenceableFlags
  { refUnknown :: [(UnknownName       , [Primitive])]
  , refBuiltIn :: [(BuiltInUnknownName, [Primitive])]
  }

getRefUnknown :: UnknownName -> ReferenceableFlags -> Maybe [Primitive]
getRefUnknown name = lookup name . refUnknown

addRefUnknown :: UnknownName -> [Primitive] -> ReferenceableFlags -> ReferenceableFlags
addRefUnknown name fs refs = refs { refUnknown = (name,fs) : (refUnknown refs) }

getRefBuiltIn :: BuiltInUnknownName -> ReferenceableFlags -> Maybe [Primitive]
getRefBuiltIn name = lookup name . refBuiltIn

addRefBuiltIn :: BuiltInUnknownName -> [Primitive] -> ReferenceableFlags -> ReferenceableFlags
addRefBuiltIn name fs refs = refs { refBuiltIn = (name,fs) : (refBuiltIn refs) }

encodeOverlapping :: [Allocator] -> CO4 EncodedAdt
encodeOverlapping allocators = return snd `ap` go (ReferenceableFlags [] []) allocators
  where
    go _ []        = error "Allocator.encodeOverlapping.go: no allocators"
    go refs allocs = do
      (refs,args) <- case maxArgs of
        0 -> return (refs,[])
        _ -> mapAccumM go refs overlappingArgs

      (refs,flags) <- case allocs of
        [Known 0 1 _]     -> return (refs, [])
        [Known i n _]     -> return (refs, map constant $ invNumeric n i)
        [BuiltInKnown fs] -> return (refs, map constant fs)

        [Unknown name _] -> do
          case getRefUnknown name refs of
            Nothing -> do fs <- allocateMaxFlags 
                          return (addRefUnknown name fs refs, fs)
            Just fs -> Exception.assert (maxFlags == length fs) 
                     $ return (refs, fs)

        [BuiltInUnknown name _] -> 
          case getRefBuiltIn name refs of
            Nothing -> do fs <- allocateMaxFlags 
                          return (addRefBuiltIn name fs refs, fs)
            Just fs -> Exception.assert (maxFlags == length fs)
                     $ return (refs, fs)

        _ -> do fs <- allocateMaxFlags
                return (refs, fs)

      adt <- make (constant True) flags args prefixfree
      return (refs, adt)

      where
        overlappingArgs = transpose $ concat $ for allocs $ \case 
          Known _ _ args -> [ args ]
          Unknown _ cons -> for cons $ \case 
            AllocateConstructor args -> args
            AllocateEmpty            -> []
          BuiltInKnown {}   -> []
          BuiltInUnknown {} -> []

        allocateMaxFlags = sequence $ replicate maxFlags primitive

        maxFlags = maximum $ for allocs $ \case 
          Known _ n _        -> bitWidth n
          Unknown _ cons     -> bitWidth $ genericLength cons 
          BuiltInKnown  fs   -> length fs
          BuiltInUnknown _ n -> n

        maxArgs = maximum $ for allocs $ \case
          Known _ _ args -> length args
          Unknown _ cons -> maximum $ for cons $ \case
            AllocateConstructor args -> length args
            AllocateEmpty            -> 0
          BuiltInKnown {}   -> 0
          BuiltInUnknown {} -> 0

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
          
    go flags pattern adt (Unknown _ cons) =
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
