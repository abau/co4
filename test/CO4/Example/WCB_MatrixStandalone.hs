module CO4.Example.WCB_MatrixStandalone where

import CO4.Prelude
import Prelude hiding ( sequence)

-- * the main constraint

constraint = design_simple
-- constraint = design_stable

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
grammar = grammar0 -- geht aber nicht für stability!

grammar1 zero one plus times costM p s =     
        sequence plus times [ part1 zero one plus times costM p s 
                            , part2 zero one plus p s 
                            ]


part1 zero one plus times costM p s = choice plus
               [ item zero one p
               , pointwise times (costM p) 
                (shift zero (gap (S (S (S Z))) zero s))
               ]
part2  zero one plus  p s = choice plus 
               [ epsilon zero one (head p : p)
               , s
               ]


grammar0 zero one plus times costM p s =     
    choice plus 
       [ item zero one p
       , sequence plus times [s, s]
       , pointwise times (costM p) (shift zero (gap (S (S (S Z))) zero s))
       ] 
  
         

-- * matrix operations (general)

type Vector a = [a]
type Matrix a = [Vector a]

upright :: Matrix a -> a
upright m = last ( head m)

vget :: [e] -> N -> f -> (e -> f) -> f
vget xs i nothing just = 
    case assertKnown xs of
        [] -> nothing
        x : xs' -> case assertKnown i of
            Z    -> just x
            S i' -> vget xs' i' nothing just 

mget :: Matrix e -> N -> N -> f -> (e -> f) -> f
mget m i j nothing just = 
    vget m i nothing ( \ row -> 
    vget row j nothing ( \ x -> just x ))

mmap :: Matrix e -> (N -> N -> e -> f) -> Matrix f
mmap m f = for (zipnats m) ( \ (i,row) ->
           for (zipnats row) ( \ (j,x) -> 
                f i j x))

mtimes :: (e -> e -> e)
       -> (e -> e -> e)
       -> Matrix e
       -> Matrix e
       -> Matrix e   
mtimes plus times a b = 
    let b' = transpose b
    in  for a  ( \ row -> 
        for b' ( \ col -> dot plus times row col ) )

dot ::  (e -> e -> e)
       -> (e -> e -> e)
       -> [e]  -> [e]
       -> e
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


-- * specific matrix operations related to grammars

-- |  (shift m) ! (i,j) ==  e ! (i+1,j-1) 
shift :: e -> Matrix e -> Matrix e
shift zero m = 
    -- dropX zero (addY zero m)
    mmap m ( \ i j x -> case j of
            Z -> zero
            S j' -> mget m (S i) j' zero id )



choice plus ms = 
    foldr (pointwise plus) (head ms) (tail ms)

sequence plus times ms = 
    foldr (mtimes plus times) (head ms) (tail ms)


costM zero p = cost_with id zero p
costM2 zero p = cost_with lift2 zero p

cost_with f zero p = 
    gap Z zero ( dropY zero ( addX zero
         ( for (zipnats p) ( \ (i, x) ->
           for (zipnats p) ( \ (j, y) -> 
               case assertKnown (ltN i j) of
                   False -> zero
                   True  -> f ( cost x y ))))))
    
addX zero m = m ++ [ map (const zero) (head m) ]
dropY zero m = map ( \ row -> zero : row ) m



{-
gap delta zero m = 
    for (zip [0..] m)   $ \ (i, row) -> 
    for (zip [0..] row) $ \ (j, x) -> 
    if i + delta <= j then x else zero
-}

gap delta zero m = 
    for (zipnats m) ( \ (i,row) -> 
    for (zipnats row) ( \ (j,x) -> 
    case assertKnown (leN (plusN i delta) j) of
        True -> x 
        False -> zero ))
      
item :: e -> e -> Primary -> Matrix e
item zero one p = 
    let p' = head p : p
    in  for (zipnats p') ( \ (i,_) ->
        for (zipnats p') ( \ (j,_) ->
            case assertKnown (eqN (S i) j) of
                False -> zero
                True  -> one  
                         ))

epsilon zero one p = 
    diag zero ( map ( \ x -> one )  p )

-- * Peano numbers (used for indexing)

data N = Z | S N 

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
gtN x y = not (leN x y)

ltN :: N -> N -> Bool
ltN x y = gtN y x

eqN x y = case x of 
    Z -> case y of
        Z -> True
        S y' -> False
    S x' -> case y of
        Z -> False
        S y' -> eqN x' y'

zipnats :: [a] -> [ (N,a) ]
zipnats xs = 
    let f n xs = case assertKnown xs of
            [] -> []
            x : xs' -> (n, x) : f (S n) xs'
    in  f Z xs



diag :: e -> [e] -> Matrix e
diag zero xs = 
    for (zipnats xs) ( \ (i,x) ->
    for (zipnats xs) ( \ (j,y) ->
        case assertKnown (eqN i j) of
            False -> zero
            True  -> x
                     ) )

for xs f = map f xs


-- * primary struct


-- | explicit binary encoding for bases

data Base = A | C | G | U deriving Show
type Primary = [Base]

cost :: Base -> Base -> Energy
cost b1 b2 = case b1 of
  A -> case b2 of { U -> two  ;           _ -> mi }
  C -> case b2 of { G -> three;           _ -> mi }
  G -> case b2 of { C -> three; U -> one; _ -> mi }
  U -> case b2 of { A -> two  ; G -> one; _ -> mi }
 
-- * secondary struct


data Paren = Open | Close | Blank

type Secondary = [ Paren ]


-- * energy

data Energy = MinusInfinity 
            | Finite Nat 
     deriving Show

mi   = MinusInfinity
zero = Finite (nat 8 0)
one  = Finite (nat 8 1)
two  = Finite (nat 8 2)
three  = Finite (nat 8 3)

{-

-- | result (first, second) best bound energy

maxbound_double :: Primary 
                -> (Energy, Energy) 
maxbound_double p = maxbound
    ( MinusInfinity, MinusInfinity )
    ( Finite (nat 8 0), MinusInfinity )
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
        Finite g -> eqNat f g

gtEnergy :: Energy -> Energy -> Bool
gtEnergy a b = not (geEnergy b a)

geEnergy :: Energy -> Energy -> Bool
geEnergy a b = case b of
  MinusInfinity -> True
  Finite b' -> case a of 
    MinusInfinity -> False
    Finite a' -> geNat a' b'

plus :: Energy -> Energy -> Energy
plus e f = case e of
  Finite x -> case f of 
    Finite y      -> Finite (maxNatp x y)
    MinusInfinity -> e
  MinusInfinity -> f

maxNatp = maxNat

times :: Energy -> Energy -> Energy
times e f = case e of
  Finite x -> case f of 
    Finite y      -> Finite (plusNatp x y)
    MinusInfinity -> f
  MinusInfinity -> e

plusNatp = plusNat

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
