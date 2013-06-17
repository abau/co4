module WCB where



data Base = A | C | G | U -- deriving Show

type Primary = [Base]

data Paren = Open | Close | Blank

type Secondary = [Paren]


{-
newtype Nat8 = Nat8 ()

nat8 :: Int -> Nat8; nat8 = undefined
geNat8 :: Nat8 -> Nat8 -> Bool ; geNat8 = undefined
maxNat8 :: Nat8 -> Nat8 -> Nat8 ; maxNat8 = undefined
plusNat8 :: Nat8 -> Nat8 -> Nat8 ; plusNat8 = undefined
-}

data Energy = MinusInfinity | Finite Nat8 --  deriving Show


-- the main constraint


main = main_simple


main_simple s p = geEnergy (bound p s) (maxbound_single p)

main_with_stability s p = case maxbound_double p of
     ( first, second ) -> gtEnergy first second -- stability
                       && geEnergy (bound p s) first

-- | result: the maximum possible energy that can be bound here
maxbound_single :: Primary -> Energy
maxbound_single p = maxbound MinusInfinity (Finite (nat8 0)) plus times cost p

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

bound :: Primary -> Secondary -> Energy
bound p s = parse [] p s

parse :: [ Base ] -> Primary -> Secondary -> Energy
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

cost :: Base -> Base -> Energy
cost b1 b2 = case b1 of
  A -> case b2 of U -> Finite (nat8 1)
                  _ -> MinusInfinity
  U -> case b2 of A -> Finite (nat8 1)
                  G -> Finite (nat8 2)
                  _ -> MinusInfinity
  G -> case b2 of U -> Finite (nat8 1)
                  C -> Finite (nat8 2)
                  _ -> MinusInfinity
  C -> case b2 of G -> Finite (nat8 2)
                  _ -> MinusInfinity

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

