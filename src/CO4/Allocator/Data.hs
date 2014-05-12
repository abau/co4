module CO4.Allocator.Data
  ( Allocator (..), AllocateConstructor (..), constructors, known )
where

import qualified Control.Exception as Exception
import           Data.Tree

data Allocator = Known { _constructorIndex :: Integer
                       , _numConstructors  :: Integer
                       , _arguments        :: [Allocator]
                       }
               | Unknown        [AllocateConstructor]
               | BuiltInKnown   [Bool]
               | BuiltInUnknown Int
               deriving (Eq,Ord)

data AllocateConstructor = AllocateConstructor [Allocator]
                         | AllocateEmpty
               deriving (Eq,Ord)

instance Show Allocator where
  show = drawTree . toTree

toTree :: Allocator -> Tree String
toTree allocator = case allocator of
  Known i n args    -> Node (unwords ["Known",show i,show n]) 
                     $ zipWith argToTree [0..] args
  Unknown cons      -> Node "Unknown" $ zipWith consToTree [0..] cons
  BuiltInKnown   bs -> Node (unwords ["BuiltInKnown", show bs]) []
  BuiltInUnknown n  -> Node (unwords ["BuiltInUnknown", show n]) []
  where
    argToTree i (Known j n args) = 
      Node (show i ++ unwords ["th argument: Known",show j,show n]) 
          $ zipWith argToTree [0..] args

    argToTree i (Unknown cons) = 
      Node (show i ++ "th argument: Unknown") $ zipWith consToTree [0..] cons

    argToTree i (BuiltInKnown fs) = 
      Node (show i ++ unwords ["th argument: BuiltInKnown",show fs]) []

    argToTree i (BuiltInUnknown n) = 
      Node (show i ++ unwords ["th argument: BuiltInUnknown",show n]) []

    consToTree i (AllocateConstructor args) = 
      Node (show i ++ "th constructor") $ zipWith argToTree [0..] args
    consToTree i AllocateEmpty = 
      Node (show i ++ "th constructor: empty") []

constructors :: [Maybe [Allocator]] -> Allocator
constructors allocs = Exception.assert (not $ null allocs) 
                    $ Unknown $ map toConstructor allocs
  where
    toConstructor Nothing     = AllocateEmpty
    toConstructor (Just args) = AllocateConstructor args

known :: Integer -> Integer -> [Allocator] -> Allocator
known = Known
