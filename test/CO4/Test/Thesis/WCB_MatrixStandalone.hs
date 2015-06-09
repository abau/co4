module CO4.Test.Thesis.WCB_MatrixStandalone where

import CO4.Prelude.Nat
import Prelude (Bool (..),Show,undefined)

--data Bool     = False | True
data List a   = Nill | Conss a (List a) deriving Show
data Pair a b = Pair a b deriving Show
data N        = Z | S N  deriving Show
data Base     = A | C | G | U deriving Show
data Paren    = Open | Close | Blank deriving Show
data Energy   = MinusInfinity | Finite Nat deriving Show


constraint :: List Paren
           -> Pair (List Base) (List (List Energy))
           -> Bool
constraint secondary u = case u of
  Pair primary e ->
    let c1 = geEnergy (boundEnergy primary secondary) (upright e)
        c2 = matrixAll eqEnergy e (energyM primary e)
        c3 = matrixAll eqEnergy e (gap (S Z) MinusInfinity e)
    in
      and2' c1 (and2' c2 c3)

{-
constraint :: List Base
           -> Pair (List Paren) (List (List Energy))
           -> Bool
constraint primary u = case u of
  Pair secondary e ->
    let c1 = geEnergy (boundEnergy primary secondary) (upright e)
        c2 = matrixAll eqEnergy e (energyM primary e)
        c3 = matrixAll eqEnergy e (gap (S Z) MinusInfinity e)
    in
      and2' c1 (and2' c2 c3)
      -}

energyM :: List Base -> List (List Energy) -> List (List Energy)
energyM p m =     
  let mInfty = MinusInfinity
  in
    sum (Conss (item mInfty zeroE p)
        (Conss (product (Conss m (Conss m Nill)))
        (Conss (pointwise timesE 
                  (costM MinusInfinity p) 
                  (matrixShift mInfty (gap (S (S (S Z))) mInfty m)))
         Nill)))

upright :: List (List a) -> a
upright m = last' (head' m)

vectorGet :: List a -> N -> b -> (a -> b) -> b
vectorGet xs i nothing just = case xs of
  Nill -> nothing
  Conss y ys -> case i of
    Z -> just y
    S j -> vectorGet ys j nothing just 

matrixGet :: List (List a) -> N -> N -> b -> (a -> b) -> b
matrixGet m i j nothing just = 
  vectorGet m i nothing (\row -> 
    vectorGet row j nothing (\x -> just x))

matrixMap :: (N -> N -> a -> b) -> List (List a) -> List (List b)
matrixMap f m = for (zipNats m) (\zippedRow ->
  case zippedRow of
    Pair i row -> for (zipNats row) (\zippedElement ->
      case zippedElement of
        Pair j x -> f i j x ))

matrixTimes :: (a -> a -> a) -> (a -> a -> a)
            -> List (List a) -> List (List a)
            -> List (List a)
matrixTimes plus times a b = 
  let b' = matrixTranspose b
      dot row col = 
        let zs = zipWith' times row col
        in 
          foldr' plus (head' zs) (tail' zs)
  in  
    for a (\row -> for b' (\col -> dot row col))

matrixTranspose :: List (List a) -> List (List a)
matrixTranspose xss = case xss of
  Nill -> Nill
  Conss row rows -> case rows of
    Nill -> map' (\x -> Conss x Nill) row
    Conss x xs -> zipWith' Conss row (matrixTranspose rows)

pointwise :: (a -> b -> c) -> List (List a) 
          -> List (List b) -> List (List c)
pointwise f a b = zipWith' (\row1 row2 ->
  zipWith' f row1 row2) a b

matrixAll :: (a -> b -> Bool) -> List (List a)
          -> List (List b) -> Bool
matrixAll f a b = and' (map' and' (pointwise f a b))

matrixShift :: a -> List (List a) -> List (List a)
matrixShift zero m = matrixMap (\i j x -> case j of
  Z -> zero
  S j' -> matrixGet m (S i) j' zero id') m

sum :: List (List (List Energy)) -> List (List Energy)
sum ms = 
  foldr' (\x y -> pointwise plusE x y) (head' ms) (tail' ms)

product :: List (List (List Energy)) -> List (List Energy)
product ms = 
  foldr' (\x y -> matrixTimes plusE timesE x y) (head' ms) (tail' ms)

costM :: Energy -> List Base -> List (List Energy)
costM zero p =
  let addX  m = append' m (Conss (map' (\x -> zero) (head' m)) Nill)
      dropY m = map' (\row -> Conss zero row) m
  in
    gap Z zero (dropY (addX
      (for (zipNats p) (\zip1 -> case zip1 of
         Pair i x -> for (zipNats p) (\zip2 -> case zip2 of
           Pair j y -> case ltN i j of
             False -> zero
             True  -> cost x y)))))

gap :: N -> a -> List (List a) -> List (List a)
gap delta zero m = 
  for (zipNats m) (\zippedRow -> case zippedRow of
    Pair i row -> for (zipNats row) (\zippedElement ->
      case zippedElement of
        Pair j x -> case leN (plusN i delta) j of
          True -> x 
          False -> zero))
      
item :: a -> a -> List Base -> List (List a)
item zero one p = 
  let p' = Conss (head' p) p
  in  
    for (zipNats p') (\zippedRow -> case zippedRow of
      Pair i row -> for (zipNats p') (\zippedElement ->
        case zippedElement of
          Pair j x -> case eqN (S i) j of
            False -> zero
            True  -> one))

plusN :: N -> N -> N
plusN x y = case x of
  Z -> y
  S x' -> S (plusN x' y)

leN :: N -> N -> Bool
leN x y = case x of 
  Z -> True
  S x' -> case y of
    Z -> False
    S y' -> leN x' y'

gtN :: N -> N -> Bool
gtN x y = not' (leN x y)

ltN :: N -> N -> Bool
ltN x y = gtN y x

eqN x y = case x of 
  Z -> case y of
    Z -> True
    S y' -> False
  S x' -> case y of
    Z -> False
    S y' -> eqN x' y'

cost :: Base -> Base -> Energy
cost b1 b2 = case b1 of
  A -> case b2 of { U -> twoE  ;            _ -> MinusInfinity }
  C -> case b2 of { G -> threeE;            _ -> MinusInfinity }
  G -> case b2 of { C -> threeE; U -> oneE; _ -> MinusInfinity }
  U -> case b2 of { A -> twoE  ; G -> oneE; _ -> MinusInfinity }

zeroE  = Finite (nat 8 0)
oneE   = Finite (nat 8 1)
twoE   = Finite (nat 8 2)
threeE = Finite (nat 8 3)

eqEnergy :: Energy -> Energy -> Bool
eqEnergy a b = case a of
  MinusInfinity -> case b of
    MinusInfinity -> True
    Finite g -> False
  Finite f -> case b of
    MinusInfinity -> False
    Finite g -> eqNat f g

geEnergy :: Energy -> Energy -> Bool
geEnergy a b = case b of
  MinusInfinity -> True
  Finite b' -> case a of 
    MinusInfinity -> False
    Finite a' -> geNat a' b'

plusE :: Energy -> Energy -> Energy
plusE e f = case e of
  Finite x -> case f of 
    Finite y -> Finite (maxNat x y)
    MinusInfinity -> e
  MinusInfinity -> f

timesE :: Energy -> Energy -> Energy
timesE e f = case e of
  Finite x -> case f of 
    Finite y -> Finite (plusNat x y)
    MinusInfinity -> f
  MinusInfinity -> e

boundEnergy :: List Base -> List Paren -> Energy
boundEnergy p s = parse Nill p s

parse :: List Base -> List Base -> List Paren -> Energy
parse stack p s = case s of
  Nill -> case stack of
    Nill -> zeroE
    Conss z zs -> MinusInfinity
  Conss y ys -> case p of
    Nill -> MinusInfinity
    Conss x xs ->
      let stack' = case y of
            Blank -> stack
            Open  -> Conss x stack
            Close -> tail' stack
          here = case y of
            Blank -> zeroE
            Open  -> zeroE
            Close -> cost (head' stack) x
      in 
        timesE here (parse stack' xs ys)

append' :: List a -> List a -> List a
append' xs ys = foldr' Conss ys xs

zipNats :: List a -> List (Pair N a)
zipNats xs = 
  let f n xs = case xs of
        Nill -> Nill
        Conss y ys -> Conss (Pair n y) (f (S n) ys)
  in
    f Z xs

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' f xs ys = case xs of
  Nill -> Nill
  Conss u us -> case ys of
    Nill -> Nill
    Conss v vs -> Conss (f u v) (zipWith' f us vs)

for :: List a -> (a -> b) -> List b
for xs f = map' f xs

map' :: (a -> b) -> List a -> List b
map' f xs = case xs of
  Nill -> Nill
  Conss y ys -> Conss (f y) (map' f ys)

last' :: List a -> a
last' xs = case xs of
  Nill -> undefined
  Conss y ys -> case ys of
    Nill -> y
    Conss z zs -> last' ys

head' :: List a -> a
head' xs = case xs of
  Nill -> undefined
  Conss y ys -> y

tail' :: List a -> List a
tail' xs = case xs of
  Nill -> Nill
  Conss y ys -> ys

and' :: List Bool -> Bool
and' xs = foldr' and2' True xs

or' :: List Bool -> Bool
or' xs = foldr' or2' False xs

foldr' :: (a -> b -> b) -> b -> List a -> b
foldr' n c xs = case xs of 
  Nill -> c
  Conss y ys -> n y (foldr' n c ys)

or2' :: Bool -> Bool -> Bool
or2' x y = case x of
  True  -> True
  False -> y

and2' :: Bool -> Bool -> Bool
and2' x y = case x of
  False -> False
  True  -> y

not' :: Bool -> Bool
not' x = case x of
  False -> True
  True  -> False

id' :: a -> a
id' x = x
