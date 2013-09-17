module CO4.CSPlib.Prob049.Constraint where

import CO4.Prelude

constraint xs (ys, zs) = 
       True
    && eqListNat xs ( merge ys zs )
    && eqNat (sum ys) (sum zs)
    && let square x = timesNat x x
       in  eqNat (sum (map square ys)) 
                 (sum (map square zs))
    
merge xs ys = case xs of
    [] -> ys
    x : xs' -> case ys of
        [] -> xs
        y : ys' -> case leNat x y of
            True  -> x : merge xs' ys
            False -> y : merge xs ys'

sum xs = case xs of
    [] -> nat 8 0
    x : xs' -> foldr plusNat x xs'

eqListNat xs ys = case xs of
    [] -> case ys of
        [] -> True
        y : ys' -> False
    x : xs' -> case ys of
        [] -> False
        y : ys' -> eqNat x y && eqListNat xs' ys'
