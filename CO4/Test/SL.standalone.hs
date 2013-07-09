module CO4.Test.SL where

import CO4.Prelude
import CO4.PreludeNat

-- * string rewriting:

type Symbol =  [ Bool ]
type Word = [ Symbol ]
type Rule = ( Word , Word )
type SRS = [ Rule ]

-- * label, then remove, then unlabel

type Interpretation = Tree (Matrix Arctic)
data Label = Label Model [ Interpretation ] [ Bool ]


-- | lex. comb. of  interpretations removes one original rule completely 
-- (that is, all its labelled versions)
constraint srs lab = case lab of
    Label mod ints remove -> 
          all ( positiveI ) ints
       && let result = labelled srs mod
              srss = map (map snd) result
              values = concat ( map (map fst) result )

              css  = map (map (comps ints)) srss
          in     not ( any (any isNone) css )
              && or remove
              && eqSymbol remove ( map ( all isGreater ) css )
              && all (\(ltop,rtop) -> eqSymbol ltop rtop) values

-- * model, labelling

type Model = Tree Func
type Func = Tree [Bool]
    
-- | produces semantically labelled version (one SRS for each rule)
-- (the type is identical but the symbols have additional bits)
-- this function raises an exception if the given structure
-- is not a model
labelled :: SRS -> Model -> [ [ ((Value,Value),Rule) ] ]
labelled srs mod =
    let ks = keys ( leftmost mod )
        labelRule u k = case u of 
            (l,r) -> case labelledW mod l k of 
                ( ltop, l' ) -> case labelledW mod r k of 
                    ( rtop, r' ) -> ((ltop,rtop),(l',r'))
    in  map ( \ u ->  map ( \ k -> labelRule u k ) ks    ) srs 

type Value = [Bool]

-- labelledW :: Model -> [Symbol] -> Value -> (Value,[Symbol])
labelledW mod w k = case assertKnown w of
            [] -> (k, [])
            x:xs' -> case labelledW mod xs' k of
                (k', ys) -> let s = get mod x
                            in  (get s k'
                                 , (labelledW_app x k') : ys )

labelledW_app x k' = x ++ assertKnown k'

-- | binary decision tree, used to map bitstrings to values
data Tree a = Leaf a | Branch (Tree a) (Tree a) -- deriving Eq


-- keys :: Tree a -> [[Bool]]
keys t = case t of
    Leaf _ -> [[]]
    Branch l r -> map (False :) (keys l) ++ map (True :) (keys r)

-- leftmost :: Tree a -> a
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
timesF :: Tree a -> Tree [Bool] -> Tree a
timesF f g = case g of
    Leaf w -> Leaf (get f w)
    Branch l r -> Branch (timesF f l) (timesF f r)

get :: Tree a -> [Bool] -> a
get t p = case assertKnown p of 
    []   -> case assertKnown t of 
         Leaf x -> x
         Branch l r -> undefined
    x:p' -> case assertKnown t of
         Leaf x -> undefined 
         Branch l r -> 
             get (assertKnown (case x of False -> l ; True -> r)) p'
             -- case x of { False -> get l p' ; True  -> get r p' } 

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

allI :: (a -> Bool) -> Tree a -> Bool
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

-- type Interpretation = Tree (Matrix Arctic)

-- iSymbol :: Interpretation -> Symbol -> Matrix Arctic
iSymbol i s = get i s


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

--iSRS i s = map (iRule i) s

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
    Finite a' -> geNat8 a' b'

plusA :: Arctic -> Arctic -> Arctic
plusA e f = case e of
  MinusInfinity -> f
  Finite x -> Finite ( case f of 
    MinusInfinity -> x 
    Finite y      -> maxNat8 x y  )

timesA :: Arctic -> Arctic -> Arctic
timesA e f = case e of
  MinusInfinity -> MinusInfinity
  Finite x -> case f of 
    MinusInfinity -> MinusInfinity
    Finite y      -> Finite (plusNat8 x y)








