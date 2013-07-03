
constraint :: Nat8 -> [Nat8] -> Bool
constraint n xs = 
     all ( \ x -> le (nat8 1) x && le x n ) xs
  && all_safe xs

all_safe :: [Nat8] -> Bool
all_safe xs = case xs of
    [] -> True
    x:xs' -> safe x xs' (nat8 1) && all_safe xs'

safe :: Nat8 -> [Nat8] -> Nat8 -> Bool
safe x ys p = case ys of
    [] -> True
    y : ys' -> no_attack x y p 
        && safe x ys' (increment p)

no_attack :: Nat8 -> Nat8 -> Nat8 -> Bool
no_attack x y p = 
    neq x y && neq (add x p) y && neq x (add y p)

le = leNat8
neq a b = not (eqNat8 a b)
add = plusNat8

increment :: Nat8 -> Nat8
increment x = add x (nat8 1)
