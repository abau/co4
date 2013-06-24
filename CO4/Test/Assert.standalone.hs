module CO4.Test.WCB_Matrix where

import CO4.Prelude
import CO4.Test.WCB_Nat8

constraint s xs = 
    eqNat8 s ( summe xs )

summe xs = case  xs of
    [] -> nat8 0
    x:xs' -> plus x (summe xs')

plus = plusNat8
