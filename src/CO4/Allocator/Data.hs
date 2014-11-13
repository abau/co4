module CO4.Allocator.Data
  ( Allocator (..), AllocateConstructor (..)
  , constructors, known, unknown, builtInKnown, builtInUnknown, allocatorId )
where

import qualified Control.Exception as Exception
import           Data.Tree

data Allocator = Known { _constructorIndex :: Int
                       , _numConstructors  :: Int
                       , _arguments        :: [Allocator]
                       }
               | Unknown        [AllocateConstructor]
               | BuiltInKnown   [Bool]
               | BuiltInUnknown Int
               | AllocatorId    Int Allocator

data AllocateConstructor = AllocateConstructor [Allocator]
                         | AllocateEmpty

instance Show Allocator where
  show = drawTree . toTree

toTree :: Allocator -> Tree String
toTree allocator = case allocator of
  Known i n args    -> Node (unwords ["Known",show i,show n]) 
                     $ zipWith argToTree [0..] args
  Unknown cons      -> Node "Unknown" $ zipWith consToTree [0..] cons
  BuiltInKnown   bs -> Node (unwords ["BuiltInKnown", show bs]) []
  BuiltInUnknown n  -> Node (unwords ["BuiltInUnknown", show n]) []
  AllocatorId id a  -> Node (unwords ["AllocatorId", show id]) [toTree a]
  where
    argToTree i (Known j n args) = 
      Node (show i ++ unwords ["th argument: Known",show j,show n]) 
          $ zipWith argToTree [0..] args

    argToTree i (Unknown cons) = 
      Node (show i ++ "th argument: Unknown") $ zipWith consToTree [0..] cons

    argToTree i (BuiltInKnown fs) = 
      Node (show i ++ unwords ["th argument: BuiltInKnown", show fs]) []

    argToTree i (BuiltInUnknown n) = 
      Node (show i ++ unwords ["th argument: BuiltInUnknown", show n]) []

    argToTree i (AllocatorId id a) =
      Node (show i ++ unwords ["th argument: AllocatorId", show id]) [toTree a]

    consToTree i (AllocateConstructor args) = 
      Node (show i ++ "th constructor") $ zipWith argToTree [0..] args
    consToTree i AllocateEmpty = 
      Node (show i ++ "th constructor: empty") []

-- |@constructors xs@ returns an allocator that represents a value
-- with @n = length xs@ constructors. The @i@-th element of @xs@
-- either constains @Just@ a list of allocators for the arguments of
-- the @i@-th constructor or @Nothing@. @Nothing@ indicates that the
-- returned allocator never generates an 'CO4.EncodedAdt.EncodedAdt' that represents
-- a value using the @i@-th constructor.
constructors :: [Maybe [Allocator]] -> Allocator
constructors allocs = Exception.assert (not $ null allocs) 
                    $ unknown $ map toConstructor allocs
  where
    toConstructor Nothing     = AllocateEmpty
    toConstructor (Just args) = AllocateConstructor args

-- |@known = @ 'Known'
known :: Int -> Int -> [Allocator] -> Allocator
known = Known

-- |@unknown = @ 'Unknown'
unknown :: [AllocateConstructor] -> Allocator
unknown cs = Unknown cs

-- |@builtInKnown = @ 'BuiltInKnown'
builtInKnown :: [Bool] -> Allocator
builtInKnown = BuiltInKnown

-- |@builtInUnknown = @ 'BuiltInUnknown'
builtInUnknown :: Int -> Allocator
builtInUnknown = BuiltInUnknown

-- |@allocatorId = @ 'AllocatorId'
allocatorId :: Int -> Allocator -> Allocator
allocatorId = AllocatorId
