
type Bit = Bool
type Nat = [Bit]

main n xs = 
     all ( \ x -> le one x && le x n ) xs
  && all_safe xs

all_safe xs = case xs of
    [] -> True
    x:xs' -> safe x xs' one && all_safe xs'

safe x ys p = case ys of
    [] -> True
    y : ys' -> no_attack x y p 
        && safe x ys' (increment p)

no_attack x y p = 
    neq x y && neq (add x p) y && neq x (add y p)

-- boilerplat for handling numbers follows ------------------

one = [True]

eqNat xs ys = le xs ys && le ys xs

neq xs ys = not (eqNat xs ys)

gt xs ys = not (le xs ys)

le xs ys = ge ys xs

ge :: Nat -> Nat -> Bool
ge xs ys = ge_run True xs ys

ge_run prev xs ys = case xs of
    [] -> prev && not ( or ys )
    x : xs' ->  case ys of
        [] -> prev || or xs
        y : ys' -> ge_run ((x && not y) || (prev && (x == y))) xs' ys'

add :: Nat -> Nat -> Nat
add xs ys = add_with False xs ys

add_with c xs ys = case xs of
    [] -> increment_with c ys
    x : xs' -> case ys of
        [] -> increment_with c xs
        y : ys' -> ( xor3 c x y ) : add_with (atleast2 c x y) xs' ys'

increment xs = increment_with True xs

increment_with c xs = case xs of
    [] -> [c]
    x : xs' -> (xor2 c x) : increment_with ( c && x) xs'

atleast2 x y z = or3 (x && y) (x && z) (y && z)

or3 :: Bit -> Bit -> Bit -> Bit
or3 x y z = x || (y || z)

xor3 :: Bit -> Bit -> Bit -> Bit
xor3 x y z = xor2 x (xor2 y z)
  
xor2 :: Bit -> Bit -> Bit
xor2 a b = case a of
  False -> b
  True  -> not b
