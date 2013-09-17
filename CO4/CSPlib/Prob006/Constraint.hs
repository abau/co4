module CO4.CSPlib.Prob006.Constraint where

import Prelude hiding ( sum )
import CO4.Prelude

constraint len diffs = 
       leNat (sum diffs) len
    && alldifferent ( map sum ( segments diffs ))


-- naive implementation
alldifferent xs = case xs of
    [] -> True
    x : ys -> all ( \ y -> not (eqNat x y)) ys
          && alldifferent ys

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
