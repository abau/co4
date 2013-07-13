module WCB where

-- import CO4.Test.WCB_Nat8

-- | explicit binary encoding for bases
data Base = Base [Bool] -- of length 2

a = Base [False, False]
c = Base [False, True]
g = Base [True, False]
u = Base [True, True]

-- data Base = A | C | G | U -- deriving Show

type Primary = [Base]

data Paren = Open | Close | Blank

type Secondary = [Paren]



data Energy = MinusInfinity | Finite Nat --  deriving Show


-- the main constraint


constraint = main_simple
-- constraint = main_with_stability


main_simple s p = geEnergy (bound p s) (maxbound_single p)

main_with_stability s p = case maxbound_double p of
     ( first, second ) -> gtEnergy first second -- stability
                       && geEnergy (bound p s) first

-- | result: the maximum possible energy that can be bound here
maxbound_single :: Primary -> Energy
maxbound_single p = maxbound MinusInfinity (Finite (nat 8 0)) plus times cost p

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
    Finite a' -> geNat a' b'

plus :: Energy -> Energy -> Energy
plus e f = case e of
  Finite x -> case f of 
    Finite y      -> Finite (maxNat x y)
    MinusInfinity -> e
  MinusInfinity -> f

times :: Energy -> Energy -> Energy
times e f = case e of
  Finite x -> case f of 
    Finite y      -> Finite (plusNat x y)
    MinusInfinity -> MinusInfinity
  MinusInfinity -> MinusInfinity

bound :: Primary -> Secondary -> Energy
bound p s = parse [] p s

parse :: [ Base ] -> Primary -> Secondary -> Energy
parse stack p s = case s of
    [] -> case stack of
         [] -> Finite (nat 8 0)
         _ -> MinusInfinity
    y:ys -> case p of
         [] -> MinusInfinity
         x:xs -> case y of
            Blank -> parse stack xs ys
            Open  -> parse (x:stack) xs ys
            Close -> case stack of
                [] -> MinusInfinity
                z : zs -> times (cost z x) (parse zs xs ys)


maxbound :: e -- ^ semi-ring zero (= forbidden energy)
         -> e -- ^ semi-ring one (= zero enery)
         -> ( e -> e -> e ) -- ^ semiring plus (= max)
         -> ( e -> e -> e ) -- ^ semiring times (= plus)
         -> ( Base -> Base -> e ) -- ^ energy bound by this pairing
         -> Primary -> e
maxbound zero one plus times cost p = case p of
  []     -> one
  (x:xs) -> case xs of
    [] -> one
    _  -> foldr plus (group zero one plus times cost p) 
                     (splittings zero one plus times cost p)

group :: e -- ^ semi-ring zero (= forbidden energy)
         -> e -- ^ semi-ring one (= zero enery)
         -> ( e -> e -> e ) -- ^ semiring plus (= max)
         -> ( e -> e -> e ) -- ^ semiring times (= plus)
         -> ( Base -> Base -> e ) -- ^ energy bound by this pairing
      -> Primary -> e
group zero one plus times cost p = case p of
  []     -> zero
  (x:xs) -> times (cost x (last' xs)) (maxbound zero one plus times cost (init' xs))

init' :: [a] -> [a]
init' xs = case xs of
  []   -> undefined
  y:ys -> case ys of
    [] -> []
    _  -> y : (init' ys)
                
last' :: [a] -> a
last' a = case a of
  []     -> undefined
  (x:xs) -> case xs of
    [] -> x
    _  -> last' xs



{-

-- | just energy 1 for each admissible base pair
cost :: Base -> Base -> Energy
cost b1 b2 = case b1 of
  A -> case b2 of U -> Finite (nat8 1)
                  _ -> MinusInfinity
  U -> case b2 of A -> Finite (nat8 1)
                  G -> Finite (nat8 1)
                  _ -> MinusInfinity
  G -> case b2 of U -> Finite (nat8 1)
                  C -> Finite (nat8 1)
                  _ -> MinusInfinity
  C -> case b2 of G -> Finite (nat8 1)
                  _ -> MinusInfinity
-}

applyB :: Base -> Tree a -> a
applyB b t = case b of
    Base bits -> applyI t bits

cost :: Base -> Base -> Energy
cost b1 b2 = applyB b2 ( applyB b1 costT )

mi = MinusInfinity
one = Finite (nat 8 1)
 
-- | FIXME: unit cost model (each base pair binds 1)
costT :: Tree (Tree Energy)
costT = basetree
    (basetree mi mi mi one) -- a u
    (basetree mi mi one mi) -- c g
    (basetree mi one mi one) -- g c, g u
    (basetree one mi one mi) -- u a, u g

basetree a c g u = 
    Branch (Branch (Leaf a)(Leaf c))
           (Branch (Leaf g)(Leaf u))


-- * binary decision tree, 
-- used to map bitstrings to values
data Tree a = Leaf a 
           | Branch (Tree a) (Tree a) 

type Index = [Bool]

applyI :: Tree a -> Index -> a
applyI t w = case t of
    Leaf u -> u
    Branch l r -> case w of
        [] -> undefined
        x:xs -> applyI (case x of
            False -> l
            True  -> r ) xs


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

