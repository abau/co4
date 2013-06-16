module CO4.Test.SL where

main = main_step

-- * string rewriting:

type Symbol =  [ Bool ]
type Word = [ Symbol ]
type Rule = ( Word , Word )
type SRS = [ Rule ]

-- * label, then remove



-- * model, labelling

type Model = [(Symbol,Func)]

isModel srs mod =
    all ( \ u -> case u of (l,r) -> eqFunc l r ) ( mSRS mod srs )

main_model srs mod = isModel srs mod
    




-- FIXME: following should be merged with Interpretation stuff below


mSymbol :: Model -> Symbol -> Func
mSymbol m s = case m of
    [] -> undefined
    tm : m' -> case tm of
        (t,m) -> -- case  t == s of  -- FIXME
                case eqSymbol t s of
             True  -> m
             False -> mSymbol m' s

mWord :: Model -> [Symbol] -> Func
mWord m w = case w of
    [] -> undefined
    x : xs -> let s = mSymbol m x 
              in case xs of
                    [] -> s
                    _  -> timesF s (mWord m xs)

mRule i u = case u of (l,r) -> (mWord i l, mWord i r)

mSRS i s = map (mRule i) s

-- | binary decision tree, used to map bitstrings to values
data BDT a = Leaf a | Branch (BDT a) (BDT a) -- deriving Eq
type Func = BDT [Bool]

eqFunc s t = case s of
    Leaf x -> case t of
        Leaf y -> eqSymbol x y -- x == y
        _ -> undefined
    Branch l r -> case t of
        Leaf y -> undefined
        Branch p q -> (eqFunc l p) && (eqFunc r q)

-- | wrong way: first g, then f  (information goes from right to left)
timesF :: BDT a -> BDT [Bool] -> BDT a
timesF f g = case g of
    Leaf w -> Leaf (applyF f w)
    Branch l r -> Branch (timesF f l) (timesF f r)

applyF :: BDT a -> [Bool] -> a
applyF f w = case f of
    Leaf u -> u
    Branch l r -> case w of
        [] -> undefined
        x:xs -> applyF (case x of
            False -> l
            True  -> r ) xs

-- * compatibility, monotonicity

data Step = Step Interpretation SRS

main_step :: SRS -> Step -> Bool
main_step srs step = case step of 
    Step int  srs'  -> 
        let ks = map (keep int) srs
        in     positiveI int
            && or (map not ( map fst ks))
            && srs' == map snd ( filter fst ks )

-- | note: partial function 
-- (raises exception if incompatible)
keep :: Interpretation -> Rule -> (Bool, Rule)
keep i u = case iRule i u of 
    (l,r) -> case gg0MA l r of
          True -> (False, u)
          False -> case geMA l r of
              True -> (True, u)
              False -> undefined -- ??

positiveI :: Interpretation -> Bool
positiveI i = allI ( \ m -> positiveM m ) i

allI :: (a -> Bool) -> BDT a -> Bool
allI prop t = case t of
    Leaf x -> prop x
    Branch l r -> allI prop l && allI prop r

positiveM :: Matrix Arctic -> Bool
positiveM m = case m of
    [] -> False
    xs : _ -> case xs of
        [] -> False
        x : _ -> finite x

gg0MA a b = and ( zipWith ( \ xs ys -> and (zipWith gg0A xs ys)  ) a b )

geMA a b = and ( zipWith ( \ xs ys -> and (zipWith geA xs ys)  ) a b )

-- * interpretation:

type Interpretation = BDT (Matrix Arctic)

iSymbol :: Interpretation -> Symbol -> Matrix Arctic
iSymbol i s = applyF i s


-- following hack makes eqSymbol monomorphic
-- so it can be used as argument for elemWith
eqSymbol p q = (p == q ) && case p of
    [] -> True ; x : xs -> case x of 
         True -> True ; False -> True

iWord i w = case w of
    [] -> undefined
    x : xs -> let m = iSymbol i x 
              in case xs of
                    [] -> m
                    _  -> timesMA m (iWord i xs)

iRule i u = case u of (l,r) -> (iWord i l, iWord i r)

iSRS i s = map (iRule i) s

-- * matrices:

type Matrix a = [[a]]


plusMA a b = zipWith (zipWith plusA) a b

timesMA a b = 
    let b' = transpose b
    in  map ( \ row -> map ( dot row ) b' ) a

transpose xss = case xss of
    [] -> []
    xs : xss' -> case xss' of
        [] -> map (\ x -> [x]) xs
        _  -> zipWith (:) xs ( transpose xss')

dot :: [Arctic] -> [Arctic] -> Arctic
dot xs ys = sumA ( zipWith timesA xs ys)

sumA xs = foldr plusA MinusInfinity xs

-- * arctic operations:

data Arctic = MinusInfinity | Finite Nat 
    -- deriving (Eq, Show)

infinite a = case a of
    MinusInfinity -> True
    _ -> False

finite a = case a of
    MinusInfinity -> False
    _ -> True

gg0A a b = gtA a b || infinite b 

gtA a b = not (geA b a)

geA a b = case b of
  MinusInfinity -> True
  Finite b' -> case a of 
    MinusInfinity -> False
    Finite a' -> ge a' b'

plusA :: Arctic -> Arctic -> Arctic
plusA e f = case e of
  Finite x -> case f of 
    Finite y      -> Finite (maxNat x y)
    MinusInfinity -> Finite x
  MinusInfinity -> f

timesA :: Arctic -> Arctic -> Arctic
timesA e f = case e of
  Finite x -> case f of 
    Finite y      -> Finite (add x y)
    MinusInfinity -> MinusInfinity
  MinusInfinity -> MinusInfinity

-- * Nat operations:

type Bit = Bool 
type Nat = [Bit]

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
    [] -> prev && not ( or ys )
    x : xs' ->  case ys of
        [] -> prev || or xs
        y : ys' -> 
           ge_run ((x && not y) || (prev && (x == y))) xs' ys'

add :: Nat -> Nat -> Nat
add xs ys = add_with False xs ys

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
