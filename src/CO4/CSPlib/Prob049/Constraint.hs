module CO4.CSPlib.Prob049.Constraint where

import CO4.Prelude

constraint (zero, one, xs) ps = 
           handle zero ( \ x -> one ) xs ps
        && handle zero ( \ x -> x ) xs ps
        && handle zero ( \ x -> timesNat x x ) xs ps

handle zero f xs ps = 
            eqP (sums zero ( zip (map f xs) ps ))

eqP (x,y) = eqNat x y

sums zero xps =  case xps of
    [] -> (zero,zero)
    xp : xps' -> case xp of
        (x,p) -> case sums zero xps' of
            (l,r) -> case p of
                    False -> (plusNat x l, r)
                    True  -> (l, plusNat x r)

