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

data Energy = MinusInfinity | Finite Nat -- deriving Show

forbidden = MinusInfinity
e0 = Finite nat0
e1 = Finite nat1
e2 = Finite nat2
e3 = Finite nat3


-- the main constraint
main e p = geEnergy (maxbound p) e


geEnergy a b = case b of
  MinusInfinity -> True
  Finite b' -> case a of 
    MinusInfinity -> False
    Finite a' -> ge a' b'

plus :: Energy -> Energy -> Energy
plus e f = case e of
  Finite x -> case f of 
    Finite y      -> Finite (maxNat x y)
    MinusInfinity -> e
  MinusInfinity -> f

times :: Energy -> Energy -> Energy
times e f = case e of
  Finite x -> case f of 
    Finite y      -> Finite (add' x y)
    MinusInfinity -> MinusInfinity
  MinusInfinity -> MinusInfinity

maxbound :: Primary -> Energy
maxbound p = case p of
  []     -> Finite []
  (x:xs) -> case xs of
    [] -> Finite []
    _  -> foldr plus (group p) (splittings p)


group :: Primary -> Energy
group p = case p of
  []     -> MinusInfinity
  (x:xs) -> case last' xs of
    Nothing' -> MinusInfinity
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

splittings :: Primary -> [Energy]
splittings p = map (\(p1,p2) -> times (maxbound p1) (maxbound p2)) 
                   (filter' (\(p1,p2) -> not (null' p1) && not (null' p2)) 
                   (zip (inits' p) (tails' p)))

null' :: [a] -> Bool
null' xs = case xs of
  [] -> True
  _  -> False

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = case xs of
  []   -> []
  y:ys -> case f y of
            False -> filter' f ys
            True -> y : (filter' f ys)

inits' :: [a] -> [[a]]
inits' xs = case xs of
  []     -> [[]]
  (x:xs) -> [] : (map (\ys -> x:ys) (inits' xs ))
  
tails' :: [a] -> [[a]]
tails' xs = case xs of
  []     -> [[]]
  (y:ys) -> xs : tails' ys
  
maxNat :: Nat -> Nat -> Nat
maxNat xs ys = case ge xs ys of
              False -> ys
              True  -> xs

minNat :: Nat -> Nat -> Nat
minNat xs ys = case ge xs ys of
              False -> xs
              True  -> ys

ge :: Nat -> Nat -> Bool
ge xs ys = foldl (\f (x,y) -> (x && (not y)) || ((not (xor x y)) && f)) True 
                  (case fill xs ys of
                      (a',b') -> zip a' b'
                  )

add' :: Nat -> Nat -> Nat
add' a b = case add a b of
  (r,c) -> r ++ [c]

add :: Nat -> Nat -> (Nat,Bit)
add a b = foldl (\(result,carry) (a,b) -> 
                    case fullAdder a b carry of
                      (r, carry') -> (result ++ [r], carry')
                ) ([],False) 
                  (case fill a b of
                      (a',b') -> zip a' b'
                  )

fill :: Nat -> Nat -> (Nat,Nat)
fill xs ys = case xs of
  []     -> case ys of []   -> ([],[])
                       v:vs -> case fill [] vs of
                                 (us',vs') -> (False : us', v : vs')
  u:us -> case ys of [] -> case fill us [] of
                             (us',vs') -> (u : us', False : vs')
                     v:vs -> case fill us vs of
                               (us',vs') -> (u:us', v:vs')

fullAdder :: Bit -> Bit -> Bit -> (Bit,Bit)
fullAdder a b carry =
  let xorAB = xor a b
  in
    ( xor xorAB carry
    , xor (a && b) (carry && xorAB)
    )

halfAdder :: Bit -> Bit -> (Bit,Bit)
halfAdder a b = (xor a b, a && b)
  
xor :: Bit -> Bit -> Bit
xor a b = case a of
  False -> b
  True  -> not b
