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

data Maybe' a = Nothing' | Just' a -- deriving Show

data Energy = MinusInfinity | Finite Nat  -- deriving Show

forbidden = MinusInfinity
e0 = Finite nat0
e1 = Finite nat1
e2 = Finite nat2
e3 = Finite nat3


-- the main constraint

-- main e p = geEnergy (maxbound p) e

main s p = geEnergy (bound p s) (maxbound p)

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



maxbound :: Primary -> Energy
maxbound p = case p of
  []     -> Finite []
  (x:xs) -> case xs of
    [] -> Finite []
    _  -> foldr plus (group p) (splittings p)        

group :: Primary -> Energy
group p = case p of
  []     -> forbidden
  (x:xs) -> case last' xs of
    Nothing' -> forbidden
    Just' l  -> case init' xs of
                  Nothing' -> MinusInfinity
                  Just' is -> times (cost x l) (maxbound is)

init' :: [a] -> Maybe' [a]
init' xs = case xs of
  []   -> Nothing'
  y:ys -> case ys of
    [] -> Just' []
    _  -> case init' ys of 
            Nothing' -> Just' []
            Just' ys' -> Just' (y : ys')
                
last' :: [a] -> Maybe' a
last' a = case a of
  []     -> Nothing'
  (x:xs) -> case xs of
    [] -> Just' x
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
splittings :: Primary -> [Energy]
splittings p = map ( \ (p1,p2) ->  times (maxbound p1) (maxbound p2)) 
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

{-

null' :: [a] -> Bool
null' xs = case xs of
  [] -> True
  _  -> False

inits' :: [a] -> [[a]]
inits' xs = case xs of
  []     -> [[]]
  (x:xs) -> [] : (map (\ys -> x:ys) (inits' xs ))
  
tails' :: [a] -> [[a]]
tails' xs = case xs of
  []     -> [[]]
  (y:ys) -> xs : tails' ys

-}
  
maxNat :: Nat -> Nat -> Nat
maxNat xs ys = case ge xs ys of
              False -> ys
              True  -> xs

minNat :: Nat -> Nat -> Nat
minNat xs ys = case ge xs ys of
              False -> xs
              True  -> ys


fold2l_pad :: (c -> a -> b -> c) 
     -> a 
     -> b
     -> c 
     -> [a] -> [b] -> c
fold2l_pad f x0 y0 accu xs ys = case xs of
            [] -> case ys of
                [] -> accu
                y : ys' -> fold2l_pad f x0 y0 (f accu x0 y) [] ys'
            x : xs' -> case ys of
                [] -> fold2l_pad f x0 y0 (f accu x y0) xs' []
                y : ys' -> fold2l_pad f x0 y0 (f accu x y) xs' ys'

fold2r_pad :: (a -> b -> c -> c) 
     -> a 
     -> b
     -> c 
     -> [a] -> [b] -> c
fold2r_pad f x0 y0 end xs ys = case xs of
            [] -> case ys of
                [] -> end
                y : ys' -> f x0 y (fold2r_pad f x0 y0 end  [] ys')
            x : xs' -> case ys of
                []      -> f x y0 (fold2r_pad f x0 y0 end xs' [] )
                y : ys' -> f x  y (fold2r_pad f x0 y0 end xs' ys')


ge :: Nat -> Nat -> Bool
ge xs ys = fold2l_pad ( \ f x y -> (x && (not y)) || ((not (xor2 x y)) && f))
              False False True xs ys

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
