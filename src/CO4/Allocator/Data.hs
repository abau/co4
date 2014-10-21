module CO4.Allocator.Data
  ( UnknownName, BuiltInUnknownName, Allocator (..), AllocateConstructor (..)
  , constructors, known, unknown, builtInKnown, builtInUnknown )
where

import           System.Mem.StableName (StableName,makeStableName)
import           System.IO.Unsafe (unsafePerformIO)
import qualified Control.Exception as Exception
import           Data.Tree

type UnknownName        = StableName [AllocateConstructor]
type BuiltInUnknownName = StableName Int

data Allocator = Known { _constructorIndex :: Int
                       , _numConstructors  :: Int
                       , _arguments        :: [Allocator]
                       }
               | Unknown        UnknownName [AllocateConstructor]
               | BuiltInKnown   [Bool]
               | BuiltInUnknown BuiltInUnknownName Int

data AllocateConstructor = AllocateConstructor [Allocator]
                         | AllocateEmpty

instance Show Allocator where
  show = drawTree . toTree

toTree :: Allocator -> Tree String
toTree allocator = case allocator of
  Known i n args     -> Node (unwords ["Known",show i,show n]) 
                      $ zipWith argToTree [0..] args
  Unknown _ cons     -> Node "Unknown" $ zipWith consToTree [0..] cons
  BuiltInKnown   bs  -> Node (unwords ["BuiltInKnown", show bs]) []
  BuiltInUnknown _ n -> Node (unwords ["BuiltInUnknown", show n]) []
  where
    argToTree i (Known j n args) = 
      Node (show i ++ unwords ["th argument: Known",show j,show n]) 
          $ zipWith argToTree [0..] args

    argToTree i (Unknown _ cons) = 
      Node (show i ++ "th argument: Unknown") $ zipWith consToTree [0..] cons

    argToTree i (BuiltInKnown fs) = 
      Node (show i ++ unwords ["th argument: BuiltInKnown",show fs]) []

    argToTree i (BuiltInUnknown _ n) = 
      Node (show i ++ unwords ["th argument: BuiltInUnknown",show n]) []

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

unknown :: [AllocateConstructor] -> Allocator
unknown cs = Unknown (unsafePerformIO $ makeStableName cs) cs

-- |@builtInKnown = @ 'BuiltInKnown'
builtInKnown :: [Bool] -> Allocator
builtInKnown = BuiltInKnown

builtInUnknown :: Int -> Allocator
builtInUnknown i = BuiltInUnknown (unsafePerformIO $ makeStableName i) i

