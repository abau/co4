module CO4.Allocator 
  ( Allocator (..), AllocateConstructor (..)
  , known, constructors, bottom
  )
where

import Control.Exception (assert)

data Allocator = Known { constructorIndex :: Int
                       , numConstructors  :: Int
                       , arguments        :: [Allocator]
                       }
               | Unknown [AllocateConstructor]

data AllocateConstructor = AllocateConstructor [Allocator]
                         | AllocateBottom

known :: Int -> Int -> [Allocator] -> Allocator
known = Known

constructors :: [Maybe [Allocator]] -> Allocator
constructors allocs = assert (not $ null allocs) $ Unknown $ map toConstructor allocs
  where
    toConstructor Nothing     = AllocateBottom
    toConstructor (Just args) = AllocateConstructor args

bottom :: AllocateConstructor
bottom = AllocateBottom
