
constraint :: Nat -> [Nat] -> Bool
constraint n xs = 
     all ( \ x -> le (nat 8 1) x && le x n ) xs
  && all_safe xs

all_safe :: [Nat] -> Bool
all_safe xs = case xs of
    [] -> True
    x:xs' -> safe x xs' (nat 8 1) && all_safe xs'

safe :: Nat -> [Nat] -> Nat -> Bool
safe x ys p = case ys of
    [] -> True
    y : ys' -> no_attack x y p 
        && safe x ys' (increment p)

no_attack :: Nat -> Nat -> Nat -> Bool
no_attack x y p = 
    neq x y && neq (add x p) y && neq x (add y p)

le = leNat
neq a b = not (eqNat a b)
add = plusNat

increment :: Nat -> Nat
increment x = add x (nat 8 1)
