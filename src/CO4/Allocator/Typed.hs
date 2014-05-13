module CO4.Allocator.Typed
  (TAllocator (toAllocator), FromKnown (..), unsafeTAllocator)
where

import CO4.Allocator.Data (Allocator)

newtype TAllocator t = TAllocator { toAllocator :: Allocator }
                       deriving (Eq,Ord,Show)

unsafeTAllocator :: Allocator -> TAllocator t
unsafeTAllocator = TAllocator

class FromKnown a where
  fromKnown :: a -> TAllocator a

instance FromKnown a => FromKnown (TAllocator a) where
  fromKnown (TAllocator a) = TAllocator a
