module CO4.Test.WCB_Nat8 where

-- | just newtype wrapper, for stricter type checking

newtype Nat8 = Nat8 Int 

nat8 :: Int -> Nat8
nat8 i = Nat8 i

geNat8 :: Nat8 -> Nat8 -> Bool 
geNat8 (Nat8 a) (Nat8 b) = a >= b

leNat8 :: Nat8 -> Nat8 -> Bool 
leNat8 (Nat8 a) (Nat8 b) = a <= b

eqNat8 :: Nat8 -> Nat8 -> Bool 
eqNat8 (Nat8 a) (Nat8 b) = a == b

maxNat8 :: Nat8 -> Nat8 -> Nat8 
maxNat8 (Nat8 a) (Nat8 b) = Nat8 (max a b)

plusNat8 :: Nat8 -> Nat8 -> Nat8  
plusNat8 (Nat8 a) (Nat8 b) = Nat8 (a + b)

