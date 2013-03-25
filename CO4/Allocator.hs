module CO4.Allocator 
  ( Allocator (..), AllocateUnknown (..), AllocateConstructor (..)
  , known, allocate, constructors, bottom
  )
where

import Data.Tree

data Allocator = Known { constructorIndex :: Int
                       , numConstructors  :: Int
                       , arguments        :: [Allocator]
                       }
               | Unknown AllocateUnknown

data AllocateUnknown     = AllocateUnknown     [AllocateConstructor]

data AllocateConstructor = AllocateConstructor [AllocateUnknown]
                         | AllocateBottom

instance Show Allocator where
  show = drawTree . allocatorToTree 

instance Show AllocateUnknown where
  show = drawTree . unknownToTree 

known :: Int -> Int -> [Allocator] -> Allocator
known = Known

allocate :: AllocateUnknown -> Allocator
allocate = Unknown

constructors :: [Maybe [AllocateUnknown]] -> AllocateUnknown
constructors = AllocateUnknown . map toConstructor
  where
    toConstructor Nothing     = AllocateBottom
    toConstructor (Just args) = AllocateConstructor args

bottom :: AllocateConstructor
bottom = AllocateBottom

allocatorToTree :: Allocator -> Tree String
allocatorToTree = \case
  Known i n args -> Node (concat ["known ",show i," of ",show n]) 
                  $ map allocatorToTree args
  Unknown u -> unknownToTree u

unknownToTree :: AllocateUnknown -> Tree String
unknownToTree (AllocateUnknown cons) = Node "unknown" $ zipWith consToTree [0..] cons
  where
    consToTree i = \case 
      AllocateConstructor args -> Node ("cons " ++ show i) $ map unknownToTree args
      AllocateBottom           -> Node ("cons " ++ show i ++ ": _|_") []
