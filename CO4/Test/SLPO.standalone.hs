module CO4.Test.SLPO where

import CO4.Prelude
import CO4.PreludeNat

-- * string rewriting:

type Symbol =  [ Bool ]
type Word = [ Symbol ]
type Rule = ( Word , Word )
type SRS = [ Rule ]



-- * label, then remove, then unlabel

data Label = Label Model [ Precedence ] [ Bool ]


-- | lex. comb. of  interpretations removes one original rule completely 
-- (that is, all its labelled versions)
constraint srs lab = case lab of
    Label mod precs remove -> 
          let result = labelled srs mod
              srss = map (map snd) result
              values = concat ( map (map fst) result )
              css  = map (map (comps precs)) srss
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

-- * path order 

type Precedence = [ Symbol ]

-- | x is greater y if we find x first in the list.
greater :: Precedence -> Symbol -> Symbol -> Bool
greater prec x y = case prec of
    [] -> False
    p : rec -> eqSymbol p x || (not (eqSymbol p y) && greater rec x y)

-- | this relies on memoization (else, it is inefficient)
greater_lpo :: Precedence -> Word -> Word -> Bool
greater_lpo prec xs ys = case xs of
    [] -> False
    x : xs' -> case ys of
        [] -> True
        y : ys' -> ( greater prec x y && greater_equal_lpo prec xs ys' )
            || (not (greater prec y x) && greater_lpo prec xs' ys' )

greater_equal_lpo prec xs ys = eqWord xs ys || greater_lpo prec xs ys

-- * arctic interpretation

data Comp = Greater | Equals | None

isNone c = case c of { None -> True ; _ -> False }
isGreater c = case c of { Greater -> True ; _ -> False }

comp :: Precedence -> Rule -> Comp
comp prec (l,r) = case greater_lpo prec l r of
    True -> Greater
    False -> case greater_equal_lpo prec l r of
        True -> Equals
        False -> None


-- following hack makes eqSymbol monomorphic
-- so it can be used as argument for elemWith
eqSymbol p q = (p == q ) && case p of
    [] -> True ; x : xs -> case x of 
         True -> True ; False -> True

eqWord xs ys = case xs of
    [] -> case ys of
        [] -> True
        y : ys' -> False
    x : xs' -> case ys of
        [] -> False
        y : ys' -> eqSymbol x y && eqWord xs' ys'

   






