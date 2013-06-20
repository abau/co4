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
main_simple s (p, t) = True 
    || geEnergy (bound p s) (maxbound_single p t)

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

leftmost :: Tree a -> a
leftmost t = case t of
     Leaf x -> x
     Branch l r -> leftmost l

rightmost :: Tree a -> a
rightmost t = case t of
     Leaf x -> x
     Branch l r -> rightmost r

assocs :: Tree a -> [([Bool],a)]
assocs t = case t of
    Leaf x -> [ ([], x) ]
    Branch l r -> 
        map ( \ (k,v) -> (False:k,v)) (assocs l)
     ++ map ( \ (k,v) -> (True :k,v)) (assocs r)

elems :: Tree a -> [a]
elems t = map snd ( assocs t )

indices :: Tree a -> [ [Bool]]
indices t = map fst ( assocs t )

apply :: Tree a -> [ Bool ] -> a
apply t w = case t of
    Leaf u -> u
    Branch l r -> case w of
        [] -> undefined
        x:xs -> apply (case x of
            False -> l
            True  -> r ) xs

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
    (basetree mi mi mi (Finite (nat8 1)) ) -- a
    (basetree mi mi (Finite (nat8 2)) mi) -- c
    (basetree mi (Finite (nat8 2)) mi (Finite(nat8 1)))
    (basetree (Finite (nat8 1)) mi (Finite (nat8 2))mi)

basetree a c g u = 
    Branch (Branch (Leaf a)(Leaf c))
           (Branch (Leaf g)(Leaf u))

-- * sequences

type Primary = Tree Base

data Paren = Open | Close | Blank

type Secondary = Tree Paren


data Energy = MinusInfinity 
            | Finite Nat8 --  deriving Show

mi = MinusInfinity

-- | result: the maximum possible energy that can be bound here
maxbound_single :: Primary -> Matrix Energy -> Energy
maxbound_single p t = 
    maxbound MinusInfinity (Finite (nat8 0)) 
             plus times cost p t

-- | result (first, second) best bound energy

{-

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
bound p s = parse [] (elems p) (elems s)

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
         -> ( Base -> Base -> e ) -- ^ energy bound by this pairing
         -> Primary 
         -> Matrix e
         -> Bool
maxbound_ok zero one plus times cost p t = 
    forall (indices p) $ \ start ->
    forall (indices p) $ \ end ->
        case less p q of
            False -> True
            True  -> 
                eqE (get t start end)
                 (foldr plus
                    (group zero one plus times cost p t start end)
                    (splittings zero one plus times cost p t start end) )

group :: e -- ^ semi-ring zero (= forbidden energy)
         -> e -- ^ semi-ring one (= zero enery)
         -> ( e -> e -> e ) -- ^ semiring plus (= max)
         -> ( e -> e -> e ) -- ^ semiring times (= plus)
         -> ( Base -> Base -> e ) -- ^ energy bound by this pairing
      -> Primary 
      -> Matrix e
      -> e
group zero one plus times cost p t = case p of
  []     -> zero
  (x:xs) -> times (cost x (last' xs)) (maxbound zero one plus times cost (init' xs))


splits :: [a] -> [([a],[a])]

-- splits xs = zip (inits xs) (tails xs)

splits xs = ( [], xs ) : case xs of
    [] -> []
    x : xs' -> map ( \ (ys,zs) -> (x : ys, zs) ) (splits xs' )

-- | max energy from all splits (in two non-empty parts)
splittings :: e -- ^ semi-ring zero (= forbidden energy)
         -> e -- ^ semi-ring one (= zero enery)
         -> ( e -> e -> e ) -- ^ semiring plus (= max)
         -> ( e -> e -> e ) -- ^ semiring times (= plus)
         -> ( Base -> Base -> e ) -- ^ energy bound by this pairing
      -> Primary -> [e]
splittings zero one plus times cost p 
       = map ( \ (p1,p2) ->  times (maxbound zero one plus times cost p1) 
                                   (maxbound zero one plus times cost p2)) 
           ( filter' ( \ (p1,p2) -> case p1 of 
               [] -> False ; _ -> case p2 of [] -> False ; _ -> True )
            ( splits p ) )

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = case xs of
  []   -> []
  y:ys -> let zs = filter' f ys
          in  case f y of
            False -> zs
            True -> y : zs



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

