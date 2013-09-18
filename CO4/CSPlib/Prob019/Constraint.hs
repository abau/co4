
{-
A magic sequence of length n is a sequence 
of integers x0 . . xn-1 between 0 and n-1, 
such that for all i in 0 to n-1, 
the number i occurs exactly xi times in the sequence. 
For instance, 6,2,1,0,0,0,1,0,0,0 is a magic sequence 
since 0 occurs 6 times in it, 1 occurs twice, ... 
-}

-- http://ianm.host.cs.st-andrews.ac.uk/CSPLib/prob/prob019/spec.html
-- http://www.minizinc.org/challenge2013/probs/nmseq/nmseq.mzn

module CO4.CSPlib.Prob019.Constraint where

import Prelude hiding ( sum )
import CO4.Prelude

constraint is xs = 
    case assertKnown is of
        [] -> undefined
        zero : is1 -> case assertKnown is1 of
            [] -> undefined
            one : is2 -> 
              all ( \ (i,x) -> 
                    eqNat (count (zero,one) i xs) x )
                  ( zip is xs )

count (zero,one) i xs = 
    foldr plusNat zero 
        (map ( \ x -> case eqNat i x of
                  False -> zero ; True -> one ) xs)

    
