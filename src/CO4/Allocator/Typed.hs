module CO4.Allocator.Typed
  (TAllocator (toAllocator), FromKnown (..), unsafeTAllocator, union, unions)
where

import CO4.Allocator.Data (Allocator (..), AllocateConstructor (..))
import CO4.Util (replaceAt)

newtype TAllocator t = TAllocator { toAllocator :: Allocator }
                       deriving (Eq,Ord,Show)

unsafeTAllocator :: Allocator -> TAllocator t
unsafeTAllocator = TAllocator

class FromKnown a where
  fromKnown :: a -> TAllocator a

instance FromKnown a => FromKnown (TAllocator a) where
  fromKnown (TAllocator a) = TAllocator a

union :: TAllocator t -> TAllocator t -> TAllocator t
union (TAllocator a1) (TAllocator a2) = unsafeTAllocator $ go a1 a2
  where
    go a1 a2 = case (a1, a2) of
      (BuiltInUnknown u1, BuiltInUnknown u2) | u1 == u2 ->
        BuiltInUnknown u1

      (BuiltInKnown k1, BuiltInKnown k2) | length k1 == length k2 ->
        if k1 == k2 
        then BuiltInKnown k1 
        else BuiltInUnknown $ length k1

      (BuiltInKnown k1, BuiltInUnknown u2) | length k1 == u2 ->
        BuiltInUnknown u2

      (BuiltInUnknown {}, BuiltInKnown {}) -> go a2 a1

      (Unknown u1, Unknown u2) | length u1 == length u2 ->
        Unknown $ zipWith goConstructors u1 u2

      (Known i1 n1 as1, Known i2 n2 as2) | n1 == n2 ->
        if i1 == i2
        then Known i1 n1 $ zipWith go as1 as2
        else Unknown $ replaceAt i1 (AllocateConstructor as1)
                     $ replaceAt i2 (AllocateConstructor as2)
                     $ replicate (fromIntegral n1) AllocateEmpty

      (Known i n as, Unknown u) | length u == n ->
        Unknown $ replaceAt i (goConstructors (AllocateConstructor as) (u !! i)) u

      (Unknown {}, Known {}) -> go a2 a1

      _ -> error $ "Allocator.Typed.union: disjunct allocators '" ++ show a1 ++ "' and '" ++ show a2 ++ "'"

    goConstructors c1 c2 = case (c1,c2) of
      (AllocateConstructor a1, AllocateConstructor a2) | length a1 == length a2 ->
        AllocateConstructor $ zipWith go a1 a2

      (AllocateConstructor a1, AllocateEmpty) -> AllocateConstructor a1
      (AllocateEmpty, AllocateConstructor a2) -> AllocateConstructor a2
      (AllocateEmpty, AllocateEmpty)          -> AllocateEmpty

unions :: [TAllocator a] -> TAllocator a
unions = foldl1 union
