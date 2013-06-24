module CO4.Test.WCB_Matrix where

import CO4.Prelude
import CO4.Test.WCB_Nat8

constraint s xs = eqNat8 s ( summe xs)

summe xs = case assertKnown xs of
    [] -> nat8 0
    x:xs' -> plusNat8 x (summe xs')
