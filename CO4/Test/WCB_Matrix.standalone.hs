module CO4.Test.WCB_Matrix where


import CO4.Prelude
import Prelude hiding (const, init, last, sequence)

-- * the main constraint

-- constraint = design_simple
constraint = design_stable

{-
ssp :: Primary -> Matrix Energy -> Bool
ssp p m = 
       all2 eqEnergy m (grammar p m)
   &&  all2 eqEnergy m (gap1 m)
-}

design_simple :: Secondary 
            -> (Primary, Matrix Energy)
            -> Bool
design_simple s (p, m) =
       geEnergy (bound p s) (upright m)
   &&  all2 eqEnergy m 
            (grammar mi zero plus times (costM mi) p m)
   &&  all2 eqEnergy m (gap (S Z) mi m)

design_stable :: Secondary 
            -> (Primary, Matrix Energy2)
            -> Bool
design_stable s (p, m) = case upright m of
   (best,second) -> 
          geEnergy (bound p s) best
      &&  gtEnergy best second
      &&  all2 eqEnergy2 m 
             (grammar (lift2 mi)(lift2 zero) plus2 times2 (costM2 (lift2 mi)) p m)
      &&  all2 eqEnergy2 m (gap (S Z) (lift2 mi) m)
   

grammar :: e -> e 
        -> (e -> e -> e) -> (e -> e -> e)
        -> (Primary -> [[e]])
        -> Primary -> Matrix e -> Matrix e
grammar zero one plus times costM p s = 
    choice plus 
       [ item zero one p
       , sequence plus times [s, s]
       , pointwise times (costM p) (shift zero (gap (S (S (S Z))) zero s))
       ]


type Matrix a = [[a]]

upright m = last ( head m)

last xs = case xs of
    [] -> undefined
    x : ys -> case ys of
        [] ->  x
        _  -> last ys

init xs = case xs of
    [] -> undefined
    x : ys -> case ys of
        [] -> []
        _  -> x : init ys

const x y = x


-- |  (shift m) ! (i,j) ==  e ! (i+1,j-1) 
shift :: e -> Matrix e -> Matrix e
shift zero m = dropX zero (addY zero m)

dropX zero m = tail m ++ [ map (const zero) (head m) ]
addY zero m = map ( \ row -> init (zero : row) ) m


mtimes :: (e -> e -> e)
       -> (e -> e -> e)
       -> Matrix e
       -> Matrix e
       -> Matrix e   
mtimes plus times a b = 
    let b' = transpose b
    in  for a  ( \ row -> 
        for b' ( \ col -> dot plus times row col ) )

dot plus times xs ys = 
              let zs = zipWith times xs ys 
              in foldr plus (head zs) (tail zs)

transpose :: Matrix e -> Matrix e
transpose xss = case assertKnown xss of
    [] -> []
    row : rows -> case assertKnown rows of
       [] -> map ( \ x -> [x] ) row
       _ -> zipWith (:) row (transpose rows)

pointwise f a b = zipWith (zipWith f) a b    

all2 f a b = and (map and (pointwise f a b))



choice plus ms = 
    foldr (pointwise plus) (head ms) (tail ms)

sequence plus times ms = 
    foldr (mtimes plus times) (head ms) (tail ms)


costM zero  p = forward zero ( dropY zero ( addX zero
         ( for p ( \ x ->
         for p ( \ y -> cost x y ) ) ) ) )

costM2 zero p = forward zero ( dropY zero ( addX zero
         ( for p ( \ x ->
         for p ( \ y -> lift2 (cost x y) ) ) ) ) )

addX zero m = map ( \ row -> zero : row ) m
dropY zero m = m ++ [ map (const zero) (head m) ]
                
-- gap1 zero m = forward zero m 
gap1 zero m  = gap (S Z) zero m

forward zero m = with_empty zero zero m

{-
gap delta zero m = 
    for (zip [0..] m)   $ \ (i, row) -> 
    for (zip [0..] row) $ \ (j, x) -> 
    if i + delta <= j then x else zero
-}

data N = Z | S N 

plusN :: N -> N -> N
plusN x y = case x of
    Z -> y
    S x' -> S (plusN x' y)

le :: N -> N -> Bool
le x y = case x of 
    Z -> True
    S x' -> case y of
        Z -> False
        S y' -> le x' y'

zipnats :: [a] -> [ (N,a) ]
zipnats xs = 
    let f n xs = case xs of
            [] -> []
            x : xs' -> (n, x) : f (S n) xs'
    in  f Z xs

gap delta zero m = 
    for (zipnats m) ( \ (i,row) -> 
    for (zipnats row) ( \ (j,x) -> 
    case assertKnown (le (plusN i delta) j) of
        True -> x 
        False -> zero ))



with_empty :: e -> e -> Matrix e -> Matrix e
with_empty zero one m = case m of
    [] -> []
    row : rows -> 
       (one : tail row)
       : map ( \ row -> zero : row) 
             (with_empty zero one (map tail rows))
        
item :: e -> e -> Primary -> Matrix e
item zero one p = dropY zero (addX zero
     ( diag zero (map ( const one ) p )) )

diag :: e -> [e] -> Matrix e
diag zero xs = case xs of
    [] -> []
    x:xs' -> let d = diag zero xs'
             in  (x : map (const zero) d) 
                 : map (\ row -> zero : row) d 

for xs f = map f xs





{-
main_with_stability s p = case maxbound_double p of
    ( first, second ) -> 
           gtEnergy first second -- stability
        && geEnergy (bound p s) first
-}

-- * primary struct


-- | explicit binary encoding for bases
data Base = Base [Bool] -- of length 2

a = Base [False, False]
c = Base [False, True]
g = Base [True, False]
u = Base [True, True]

type Primary = [ Base ]


eqBase b c = case b of 
    Base xs -> case c of
        Base ys -> eqList eqBool xs ys

eqList eq xs ys = case xs of
    [] -> case ys of
         [] -> True
         _  -> False
    x:xs' -> case ys of
         [] -> False
         y:ys' -> eq x y && eqList eq xs' ys'


applyB :: Base -> Tree a -> a
applyB b t = case b of
    Base bits -> applyIU t bits

cost :: Base -> Base -> Energy
cost b1 b2 = applyB b1 (applyB b2 costT)
 
-- | FIXME: unit cost model (each base pair binds 1)
costT :: Tree (Tree Energy)
costT = basetree
    (basetree mi  mi    mi    two) -- a u
    (basetree mi  mi    three mi) -- c g
    (basetree mi  three mi    one) -- g c, g u
    (basetree two mi    one   mi) -- u a, u g

basetree a c g u = 
    Branch (Branch (Leaf a)(Leaf c))
           (Branch (Leaf g)(Leaf u))

-- * secondary struct


data Paren = Open | Close | Blank

type Secondary = [ Paren ]


-- * energy

data Energy = MinusInfinity 
            | Finite Nat8 
     --deriving Show

mi   = MinusInfinity
zero = Finite (nat8 0)
one  = Finite (nat8 1)
two  = Finite (nat8 2)
three  = Finite (nat8 3)



{-

-- | result (first, second) best bound energy

maxbound_double :: Primary 
                -> (Energy, Energy) 
maxbound_double p = maxbound
    ( MinusInfinity, MinusInfinity )
    ( Finite (nat8 0), MinusInfinity )
    ( \ (f1, s1) (f2, s2) -> ( maxEnergy f1 f2
                             , maxEnergy ( minEnergy f1 f2 )
                                 (maxEnergy s2 s2 ) ) )
    ( \ (f1, s1) (f2, s2) -> ( times f1 f2 , maxEnergy (times f1 s2)(times f2 s1) ) )
    ( \ x y -> ( cost x y , MinusInfinity ) )
    p

-}

maxEnergy a b = case geEnergy a b of
    False -> b
    True  -> a

minEnergy a b = case geEnergy a b of
    False -> a
    True  -> b

eqEnergy :: Energy -> Energy -> Bool
eqEnergy a b = case a of
    MinusInfinity -> case b of
        MinusInfinity -> True
        Finite g -> False
    Finite f -> case b of
        MinusInfinity -> False
        Finite g -> eqNat8 f g

gtEnergy :: Energy -> Energy -> Bool
gtEnergy a b = not (geEnergy b a)

geEnergy :: Energy -> Energy -> Bool
geEnergy a b = case b of
  MinusInfinity -> True
  Finite b' -> case a of 
    MinusInfinity -> False
    Finite a' -> geNat8 a' b'

plus :: Energy -> Energy -> Energy
plus e f = case e of
  Finite x -> case f of 
    Finite y      -> Finite (maxNat8p x y)
    MinusInfinity -> e
  MinusInfinity -> f

maxNat8p = maxNat8

times :: Energy -> Energy -> Energy
times e f = case e of
  Finite x -> case f of 
    Finite y      -> Finite (plusNat8p x y)
    MinusInfinity -> f
  MinusInfinity -> e

plusNat8p = plusNat8

-- * Pairs of energies

type Energy2 = (Energy, Energy)

eqEnergy2 (f1,s1) (f2,s2) = 
    eqEnergy f1 f2 && eqEnergy s1 s2

lift2 f = (f, mi)

plus2 :: Energy2 -> Energy2 -> Energy2 
plus2 (f1,s1) (f2,s2) = 
    ( plus f1 f2 
    , plus (minEnergy f1 f2) (plus s1 s2)
    )

times2 :: Energy2 ->  Energy2 ->  Energy2 
times2 (f1,s1) (f2,s2) = 
    ( times f1 f2 
    , plus (times f1 s2) (times f2 s1)
    )

-- * compute bound energy for a given secondary str.

bound :: Primary -> Secondary -> Energy
bound p s = parse [] p s

-- | note: secondary is completely known, 
-- thus stack height is always known
-- length of primary is known
parse :: [ Base ] -> [Base] -> [Paren] -> Energy
parse stack p s = case assertKnown s of
    [] -> case assertKnown stack of
         [] -> zero
         _ -> mi
    y:ys -> case assertKnown p of
         [] -> mi
         x:xs ->
            let stack' = case assertKnown y of
                    Blank -> stack
                    Open  -> x : stack
                    Close -> tail stack
                here = case y of
                    Blank -> zero
                    Open  -> zero
                    Close -> cost (head stack) x
            in  times here ( parse stack' xs ys)


-- * binary decision tree, 
-- used to map bitstrings to values
data Tree a = Leaf a 
           | Branch (Tree a) (Tree a) 

type Index = [Bool]

eqBool x y = case x of
    False -> not y
    True  -> y

-- | use this if the index is statically known
applyI :: Tree a -> Index -> a
applyI t w = case t of
    Leaf u -> u
    Branch l r -> case assertKnown w of
        [] -> undefined
        x:xs -> case assertKnown x of
            False -> applyI l xs
            True  -> applyI r xs

-- | use this if the index is not statically known
-- (used for cost of bases)
applyIU :: Tree a -> Index -> a
applyIU t w = case t of
    Leaf u -> u
    Branch l r -> case assertKnown w of
        [] -> undefined
        x:xs -> applyIU (case x of
            False -> l
            True  -> r  ) xs

get :: Tree (Tree a) -> Index -> Index -> a
get t i j = applyI (applyI t i) j


