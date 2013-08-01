module CO4.AllocatorData
  ( Allocator (..), AllocateConstructor (..)
  , known, constructors, empty
  )
where

import qualified Control.Exception as Exception
import           Data.Tree

data Allocator = Known { _constructorIndex :: Int
                       , _numConstructors  :: Int
                       , _arguments        :: [Allocator]
                       }
               | Unknown [AllocateConstructor]
               deriving (Eq,Ord)

data AllocateConstructor = AllocateConstructor [Allocator]
                         | AllocateEmpty
               deriving (Eq,Ord)

instance Show Allocator where
  show = drawTree . toTree

toTree :: Allocator -> Tree String
toTree allocator = case allocator of
  Known i n args -> Node (unwords ["Known",show i,show n]) 
                  $ zipWith argToTree [0..] args
  Unknown cons   -> Node "Unknown" $ zipWith consToTree [0..] cons
  where
    argToTree i (Known j n args) = 
      Node (show i ++ unwords ["th argument: known",show j,show n]) 
          $ zipWith argToTree [0..] args
    argToTree i (Unknown cons) = 
      Node (show i ++ "th argument: unknown") $ zipWith consToTree [0..] cons

    consToTree i (AllocateConstructor args) = 
      Node (show i ++ "th constructor") $ zipWith argToTree [0..] args
    consToTree i AllocateEmpty = 
      Node (show i ++ "th constructor: empty") []

known :: Int -> Int -> [Allocator] -> Allocator
known = Known

constructors :: [Maybe [Allocator]] -> Allocator
constructors allocs = Exception.assert (not $ null allocs) 
                    $ Unknown $ map toConstructor allocs
  where
    toConstructor Nothing     = AllocateEmpty
    toConstructor (Just args) = AllocateConstructor args

empty :: AllocateConstructor
empty = AllocateEmpty
