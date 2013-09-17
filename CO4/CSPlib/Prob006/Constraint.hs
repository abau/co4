module CO4.CSPlib.Prob006.Constraint where

import Prelude hiding ( sum )
import CO4.Prelude

constraint len diffs = 
       leNat (sum diffs) len
    && alldifferent 
        -- ( map sum ( segments diffs ))
       ( map_sum_segments diffs )


-- naive implementation
alldifferent xs = case xs of
    [] -> True
    x : ys -> all ( \ y -> not (eqNat x y)) ys
          && alldifferent ys

-- equivalent to map sum $ segments xs
-- but used fewer additions (?)
map_sum_segments xs = case xs of
    [] -> []
    x : xs' -> map_sum_inits xs ++ map_sum_segments xs'

map_sum_inits xs = case xs of
    [] -> []
    x : xs' -> x : map ( \ y -> plusNat x y )
                       (map_sum_inits xs')

-- non-empty contiguous subwords
segments :: [a] -> [[a]]
segments xs = filter (\ ys -> not ( null ys))
     ( concat (map inits (tails xs) ))

inits :: [a] -> [[a]]
inits xs = 
    foldr ( \ x yss -> [] : map ( \ ys -> x : ys) yss)
          [[]] xs

tails :: [a] -> [[a]]
tails xs = xs : case xs of
    [] -> []
    x : xs' -> tails xs'
    
sum xs = case xs of
    [] -> undefined
    x : xs' -> foldr plusNat x xs'
