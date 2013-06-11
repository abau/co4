module Factors where

type Bit = Bool
type Nat = [Bit]

main foo args = case args of
    (a,b) -> gt a one 
          && gt b one 
          && eqNat foo (mult a b)

one :: Nat
one = [True ]

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



or' xs = case xs of
    [] -> False
    x : xs' -> x || or' xs'

pow :: Nat -> Nat -> Nat 
pow xs ys = case ys of
    [] -> [True]
    y : ys' -> 
        let zs = pow xs ys'
            zs2 = mult zs zs
        in  case y of
                False -> zs2
                True  -> mult xs zs2

mult :: Nat -> Nat -> Nat
mult xs ys = case xs of
    [] -> []
    x : xs' -> add' (switch x ys) 
               (False : mult xs' ys)

switch x ys = map ( \ y -> x && y ) ys

add' :: Nat -> Nat -> Nat
add' xs ys = add_with False xs ys

add_with c xs ys = case xs of
    [] -> increment_with c ys
    x : xs' -> case ys of
        [] -> increment_with c xs
        y : ys' -> ( xor3 c x y ) : add_with (atleast2 c x y) xs' ys'

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
