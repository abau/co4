module CO4.Test.WCB_Matrix where

import CO4.Test.WCB_Nat8

-- * the main constraint

main = main_simple

type Matrix a = Tree (Tree a)

-- | additional parameter t is the ADP matrix
-- for the primary

main_simple :: Secondary 
            -> (Primary, Matrix Energy)
            -> Bool
main_simple s (p, t) = True ||
   (  maxbound_ok MinusInfinity (Finite (nat8 0)) 
             plus times eqEnergy cost p t
    && geEnergy (bound p s) (rightmost (leftmost t ))
   )

{-
main_with_stability s p = case maxbound_double p of
    ( first, second ) -> 
           gtEnergy first second -- stability
        && geEnergy (bound p s) first
-}

-- | binary decision tree, 
-- used to map bitstrings to values
data Tree a = Leaf a 
           | Branch (Tree a) (Tree a) 

type Index = [Bool]

lessI xs ys = case xs of
    [] -> case ys of
        []  -> False
        _ -> True
    x:xs -> case ys of
        [] -> False
        y:ys -> (not x && y) 
             || ((not x || y) && lessI xs ys )

leftmost :: Tree a -> a
leftmost t = case t of
     Leaf x -> x
     Branch l r -> leftmost l

leftmostI :: Tree a -> Index
leftmostI t = case t of
     Leaf x -> []
     Branch l r -> False : leftmostI l

rightmost :: Tree a -> a
rightmost t = case t of
     Leaf x -> x
     Branch l r -> rightmost r

rightmostI :: Tree a -> Index
rightmostI t = case t of
     Leaf x -> []
     Branch l r -> True : rightmostI r

assocs :: Tree a -> [(Index,a)]
assocs t = case t of
    Leaf x -> [ ([], x) ]
    Branch l r -> 
        map ( \ (k,v) -> (False:k,v)) (assocs l)
     ++ map ( \ (k,v) -> (True :k,v)) (assocs r)

elems :: Tree a -> [a]
elems t = map snd ( assocs t )

indices :: Tree a -> [Index]
indices t = map fst ( assocs t )

subtree :: Tree a -> Index -> Tree a
subtree t w = case w of
        [] -> t
        x:xs -> subtree (case t of 
            Leaf _ -> undefined
            Branch l r -> case x of
                False -> l
                True  -> r ) xs

apply :: Tree a -> Index -> a
apply t w = case t of
    Leaf u -> u
    Branch l r -> case w of
        [] -> undefined
        x:xs -> apply (case x of
            False -> l
            True  -> r ) xs

get :: Tree (Tree a) -> Index -> Index -> a
get t i j = apply (apply t i) j

-- | explicit binary encoding for bases
data Base = Base [Bool] -- of length 2

a = Base [False, False]
c = Base [False, True]
g = Base [True, False]
u = Base [True, True]

applyB :: Base -> Tree a -> a
applyB b t = case b of
    Base bits -> apply t bits

cost :: Base -> Base -> Energy
cost b1 b2 = applyB b2 ( applyB b1 costT )
 
-- | FIXME: these costs are arbitrary:
costT :: Tree (Tree Energy)
costT = basetree
    (basetree mi mi mi one) -- a u
    (basetree mi mi one mi) -- c g
    (basetree mi one mi one) -- g c, g u
    (basetree one mi one mi) -- u a, u g

basetree a c g u = 
    Branch (Branch (Leaf a)(Leaf c))
           (Branch (Leaf g)(Leaf u))

-- * sequences

type Primary = Tree Base

data Paren = Open | Close | Blank

type Secondary = [ Paren ]


data Energy = MinusInfinity 
            | Finite Nat8 --  deriving Show

mi = MinusInfinity
zero = Finite (nat8 0)
one = Finite (nat8 1)



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

eqEnergy a b = case a of
    MinusInfinity -> case b of
        MinusInfinity -> True
        Finite g -> False
    Finite f -> case b of
        MinusInfinity -> False
        Finite g -> (geNat8 f g) && (leNat8 f g)

gtEnergy a b = not (geEnergy b a)

geEnergy a b = case b of
  MinusInfinity -> True
  Finite b' -> case a of 
    MinusInfinity -> False
    Finite a' -> geNat8 a' b'

plus :: Energy -> Energy -> Energy
plus e f = case e of
  Finite x -> case f of 
    Finite y      -> Finite (maxNat8 x y)
    MinusInfinity -> Finite x
  MinusInfinity -> f

times :: Energy -> Energy -> Energy
times e f = case e of
  Finite x -> case f of 
    Finite y      -> Finite (plusNat8 x y)
    MinusInfinity -> MinusInfinity
  MinusInfinity -> MinusInfinity

-- * compute bound energy for a given secondary str.

bound :: Primary -> Secondary -> Energy
bound p s = parse [] (elems p) s

parse :: [ Base ] -> [Base] -> [Paren] -> Energy
parse stack p s = case s of
    [] -> case stack of
         [] -> Finite (nat8 0)
         _ -> MinusInfinity
    y:ys -> case p of
         [] -> MinusInfinity
         x:xs -> case y of
            Blank -> parse stack xs ys
            Open  -> parse (x:stack) xs ys
            Close -> case stack of
                [] -> mi
                z : zs -> 
                    times (cost z x) (parse zs xs ys)


-- | check that the given matrix is the correct ADP
-- matrix for the computation of the max. energy


maxbound_ok
         :: e -- ^ semi-ring zero (= forbidden energy)
         -> e -- ^ semi-ring one (= zero enery)
         -> ( e -> e -> e ) -- ^ semiring plus (= max)
         -> ( e -> e -> e ) -- ^ semiring times (= plus)
         -> ( e -> e -> Bool ) -- ^  equality
         -> ( Base -> Base -> e ) -- ^ energy bound by this pairing
         -> Primary 
         -> Matrix e
         -> Bool
maxbound_ok zero one plus times eq cost p t = 
    forall (indices p) ( \ start ->
    forall (indices p) ( \ end ->
        (lessI end start) ||
            eq (get t start end)
                ( foldr plus one
                 ( group times cost p t start end
                 : splittings times cost p t start end)
                )
    ))

forall xs prop = all prop xs

group :: (e -> e -> e) 
      -> (Base -> Base -> e)
      -> Primary
      -> Matrix e
      -> Index -> Index 
      -> e
group times cost p t start end = 
    times (cost (apply p start) (apply p end))
          (get t (next p start) (previous p end))


-- | energies from all splits 
-- (in two non-empty parts)
splittings times cost p t start end =
    map ( \ mid -> 
              times (get t start mid)
                    (get t (next p mid) end)
        ) ( between p start end )

between p start end = 
    filter ( \ q -> lessI start q && lessI q end )
           ( indices p )


-- | return [] for overflow
incrementI :: Index -> Index
incrementI i = case i of
    [] -> undefined
    x : xs -> case incrementI xs of
         [] -> case x of
              False -> [ True ]
              True -> []
         ys -> x : ys

-- | return [] for overflow
decrementI :: Index -> Index
decrementI i = case i of
    [] -> undefined
    x : xs -> case decrementI xs of
         [] -> case x of
              False -> [ ]
              True -> [False]
         ys -> x : ys

next  :: Tree a -> Index -> Index
next t p = case incrementI p of
    [] -> undefined
    q : qs  -> leftmostI ( subtree t (q:qs) )

previous :: Tree a -> Index -> Index
previous t p = case decrementI p of
    [] -> undefined
    q : qs  -> rightmostI ( subtree t (q:qs) )

        
    


-- | balanced binary fold 
foldb :: b -> (a -> b) -> (b -> b -> b) -> [a] -> b
foldb n e z xs = case xs of
    [] -> n
    x : xs' -> case xs' of
         [] -> e x
         _  -> case distribute xs of 
                  (ys, zs) -> z (foldb n e z ys) (foldb n e z zs)

-- | create two lists of nearly equal length
distribute :: [a] -> ( [a], [a] )
distribute xs = case xs of
    [] -> ( [], [] )
    x:xs' -> case distribute xs' of (ys,zs) -> (x : zs, ys)
