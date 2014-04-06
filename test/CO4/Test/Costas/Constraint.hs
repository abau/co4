module CO4.Test.Costas.Constraint where

import Prelude hiding ( sum )
import CO4.Prelude

import Data.List ( sort, tails )

-- | the common (obfuscated) definition is, e.g., at
-- http://mathworld.wolfram.com/CostasArray.html

-- | below is a more direct version.

type Point = (Int,Int)

diff :: Point -> Point -> Point
diff (ax, ay) (bx, by) = (ax - bx, ay - by)

check6 = costas [(1,1),(2,2),(3,5),(4,4),(5,6),(6,3)]

costas :: [ Point ] -> Bool
costas points = 
    let n = length points
    in     is_permutation (map fst points) [ 1 .. n ]
        && is_permutation (map snd points) [ 1 .. n ]
        && all_different ( do (p : qs) <- tails points ; q <- qs ; return ( diff p q ) )

is_permutation :: Ord a => [a] -> [a] -> Bool
is_permutation xs ys = sort xs == sort ys

all_different :: Ord a => [a] -> Bool
all_different xs = neighbours_different (sort xs)

neighbours_different :: Eq a => [a] -> Bool
neighbours_different xs = 
    all ( \ (x,y) -> not (x == y) ) $ zip xs (tail xs)

constraint :: Bool -> [Point] -> Bool
constraint dummy points = dummy && costas points
