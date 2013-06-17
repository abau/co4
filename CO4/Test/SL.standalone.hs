module CO4.Test.SL where

-- * string rewriting:

type Symbol =  [ Bool ]
type Word = [ Symbol ]
type Rule = ( Word , Word )
type SRS = [ Rule ]

-- * label, then remove, then unlabel

data Label = Label Model [ Interpretation ] [ Bool ]


-- | lex. comb. of  interpretations removes one original rule completely 
-- (that is, all its labelled versions)
main srs lab = case lab of
    Label mod ints remove -> 
          all ( positiveI ) ints
       && let srss = labelled srs mod
              css  = map (map (comps ints)) srss
          in     not ( any (any isNone) css )
              && or remove
              && eqSymbol remove ( map ( all isGreater ) css )

-- * model, labelling

type Model = BDT Func

    
-- | produces semantically labelled version (one SRS for each rule)
-- (the type is identical but the symbols have additional bits)
-- this function raises an exception if the given structure
-- is not a model
-- labelled :: SRS -> Model -> [SRS]
labelled srs mod =
    let ks = keys ( leftmost mod )
        labelRule u k = case u of 
            (l,r) -> case labelledW mod l k of 
                ( ltop, l' ) -> case labelledW mod r k of 
                    ( rtop, r' ) -> case eqSymbol ltop rtop of
                          True -> ( l', r' )
                          False -> undefined
    in  map ( \ u ->  map ( \ k -> labelRule u k ) ks    ) srs 

type Value = [Bool]

-- labelledW :: Model -> [Symbol] -> Value -> (Value,[Symbol])
labelledW mod w k = case w of
            [] -> (k, [])
            x:xs' -> case labelledW mod xs' k of
                (k', ys) -> let s = applyF mod x
                            in  (applyF s k', (x ++ k') : ys )


-- | binary decision tree, used to map bitstrings to values
data BDT a = Leaf a | Branch (BDT a) (BDT a) -- deriving Eq
type Func = BDT [Bool]

-- keys :: BDT a -> [[Bool]]
keys t = case t of
    Leaf _ -> [[]]
    Branch l r -> map (False :) (keys l) ++ map (True :) (keys r)

-- leftmost :: BDT a -> a
leftmost t = case t of
    Leaf x -> x
    Branch l r -> leftmost l

eqFunc s t = case s of
    Leaf x -> case t of
        Leaf y -> eqSymbol x y -- x == y
        _ -> undefined
    Branch l r -> case t of
        Leaf y -> undefined
        Branch p q -> (eqFunc l p) && (eqFunc r q)

-- | wrong way: first g, then f  (information goes from right to left)
-- timesF :: BDT a -> BDT [Bool] -> BDT a
timesF f g = case g of
    Leaf w -> Leaf (applyF f w)
    Branch l r -> Branch (timesF f l) (timesF f r)

-- applyF :: BDT a -> [Bool] -> a
applyF f w = case f of
    Leaf u -> u
    Branch l r -> case w of
        [] -> undefined
        x:xs -> applyF (case x of
            False -> l
            True  -> r ) xs

-- * lex. combination


-- comps :: [ Interpretation ] -> Rule -> Comp
comps is u = lexi (map (\ i -> comp i u) is)

-- lexi :: [Comp] -> Comp
lexi cs = case cs of
    [] -> Equals
    c : cs' -> case c of
        Greater -> Greater
        Equals -> lexi cs'
        None -> None

-- * arctic interpretation

data Comp = Greater | Equals | None

isNone c = case c of { None -> True ; _ -> False }
isGreater c = case c of { Greater -> True ; _ -> False }

comp :: Interpretation -> Rule -> Comp
comp i u = case iRule i u of
    (l,r) -> case gg0MA l r of
        True ->  Greater
        False -> case geMA l r of
            True -> Equals
            False -> None


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

-- iSymbol :: Interpretation -> Symbol -> Matrix Arctic
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

data Arctic = MinusInfinity | Finite Nat8
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

maxNat :: Nat8 -> Nat8 -> Nat8
maxNat xs ys = case ge xs ys of
              False -> ys
              True  -> xs

minNat :: Nat8 -> Nat8 -> Nat8
minNat xs ys = case ge xs ys of
              False -> xs
              True  -> ys

ge x y = geNat8 x y

add x y = plusNat8 x y


