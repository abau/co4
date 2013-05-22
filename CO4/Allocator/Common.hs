module CO4.Allocator.Common
  ( Allocator (..), AllocateConstructor (..)
  , known, constructors, bottom
  )
where

import qualified Control.Exception as Exception

data Allocator = Known { _constructorIndex :: Int
                       , _numConstructors  :: Int
                       , _arguments        :: [Allocator]
                       }
               | Unknown [AllocateConstructor]
               deriving (Eq,Ord,Show)

data AllocateConstructor = AllocateConstructor [Allocator]
                         | AllocateBottom
               deriving (Eq,Ord,Show)

known :: Int -> Int -> [Allocator] -> Allocator
known = Known

constructors :: [Maybe [Allocator]] -> Allocator
constructors allocs = Exception.assert (not $ null allocs) 
                    $ Unknown $ map toConstructor allocs
  where
    toConstructor Nothing     = AllocateBottom
    toConstructor (Just args) = AllocateConstructor args

bottom :: AllocateConstructor
bottom = AllocateBottom
