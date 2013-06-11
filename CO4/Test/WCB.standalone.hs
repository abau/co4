module WCB where

data Base = A | C | G | U -- deriving Show

type Primary = [Base]

data Paren = Open | Close | Blank

type Secondary = [Paren]

type Bit = Bool
type Nat = [Bit]

nat0 = []
nat1 = [True]
nat2 = [False,True] -- lsb is in the head
nat3 = [True,True]

data Energy = MinusInfinity | Finite Nat --  deriving Show

forbidden = MinusInfinity
e0 = Finite nat0
e1 = Finite nat1
e2 = Finite nat2
e3 = Finite nat3


-- the main constraint


-- main e p = geEnergy (maxbound p) e


-- main s p = geEnergy (bound p s) (maxbound_single p)
main s p = case maxbound_double p of
     ( first, second ) -> gtEnergy first second -- stability
                       && geEnergy (bound p s) first

-- | result: the maximum possible energy that can be bound here
maxbound_single :: Primary -> Energy
maxbound_single p = maxbound MinusInfinity (Finite []) plus times cost p

-- | result (first, second) best bound energy
maxbound_double :: Primary 
                -> (Energy, Energy) 
maxbound_double p = maxbound
    ( MinusInfinity, MinusInfinity )
    ( Finite [], MinusInfinity )
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
    Finite a' -> ge a' b'

plus :: Energy -> Energy -> Energy
plus e f = case e of
  Finite x -> case f of 
    Finite y      -> Finite (maxNat x y)
    MinusInfinity -> Finite x
  MinusInfinity -> f

times :: Energy -> Energy -> Energy
times e f = case e of
  Finite x -> case f of 
    Finite y      -> Finite (add' x y)
    MinusInfinity -> MinusInfinity
  MinusInfinity -> MinusInfinity

bound :: Primary -> Secondary -> Energy
bound p s = parse [] p s

parse :: [ Base ] -> Primary -> Secondary -> Energy
parse stack p s = case s of
    [] -> case stack of
         [] -> e0
         _ -> forbidden
    y:ys -> case p of
         [] -> forbidden
         x:xs -> case y of
            Blank -> parse stack xs ys
            Open  -> parse (x:stack) xs ys
            Close -> case stack of
                [] -> forbidden
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

cost :: Base -> Base -> Energy
cost b1 b2 = case b1 of
  A -> case b2 of U -> e1
                  _ -> forbidden
  U -> case b2 of A -> e1
                  G -> e1
                  _ -> forbidden
  G -> case b2 of U -> e1
                  C -> e2
                  _ -> forbidden
  C -> case b2 of G -> e2
                  _ -> forbidden

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

maxNat :: Nat -> Nat -> Nat
maxNat xs ys = case ge xs ys of
              False -> ys
              True  -> xs

minNat :: Nat -> Nat -> Nat
minNat xs ys = case ge xs ys of
              False -> xs
              True  -> ys


ge :: Nat -> Nat -> Bool
ge xs ys = ge_run True xs ys

ge_run prev xs ys = case xs of
    [] -> prev && not ( or' ys )
    x : xs' ->  case ys of
        [] -> prev
        y : ys' -> ge_run ((x && not y) || (prev && (x == y))) xs' ys'

or' xs = case xs of
    [] -> False
    x : xs' -> x || or' xs'

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
