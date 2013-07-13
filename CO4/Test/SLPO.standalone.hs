module CO4.Test.SLPO where

import CO4.Prelude
import CO4.PreludeNat

-- * string rewriting:

type Symbol =  [ Bool ]
type Word = [ Symbol ]

data Mode = Strict | Weak deriving ( Eq, Show )
data Rule = Rule Mode Word Word deriving ( Eq, Show )
mode :: Rule -> Mode ; mode (Rule m l r) = m
lhs :: Rule -> Word ; lhs (Rule m l r) = l
rhs :: Rule -> Word ; rhs (Rule m l r) = r

type SRS = [ Rule ]



-- * label, then remove, then unlabel

data Label = Label Model [ QP ] [ Bool ]


-- | lex. comb. of  interpretations removes one original rule completely 
-- (that is, all its labelled versions)
constraint srs lab = case lab of
    Label mod qps remove -> 
          let result = labelled srs mod
              srss = map (map snd) result
              values = concat ( map (map fst) result )
              css  = map (map (comps qps  )) srss
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
        labelRule u k = case labelledW mod (lhs u) k of 
                ( ltop, l' ) -> case labelledW mod (rhs u) k of 
                    ( rtop, r' ) -> ((ltop,rtop),Rule (mode u) l' r')
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

data Comp = Greater | GreaterEquals | None deriving Show

isNone c = case c of { None -> True ; _ -> False }
isGreaterEquals c = case c of { GreaterEquals -> True ; _ -> False }
isGreater c = case c of { Greater -> True ; _ -> False }



comps :: [ QP ] -> Rule -> Comp
comps qps u = lexi (map ( \ qp -> comp qp u) qps )

-- | this is inefficient (merging creates unknown tree nodes)
comp_1 :: QP -> Rule -> Comp
comp_1 qp u = 
    let directed w = case direction qp of
             Original -> w ; Reversed -> reverse w
    in  compareW (\ x -> get (delete qp) x)
                 (compareS (order qp)) (directed (lhs u)) (directed (rhs u))

comp :: QP -> Rule -> Comp
comp qp u = case direction qp of
    Original -> compareW (\ x -> get (delete qp) x)
                         (compareS (order qp)) (lhs u) (rhs u)
    Reversed -> compareW (\ x -> get (delete qp) x)
                         (compareS (order qp)) (reverse (lhs u)) (reverse (rhs u))

lexi :: [Comp] -> Comp
lexi cs = case cs of
    [] -> GreaterEquals
    c : cs' -> case c of
        Greater -> Greater
        GreaterEquals -> lexi cs'
        None -> None

-- * path order 

data Direction = Original | Reversed

data QP = 
     QP Direction 
        (Tree Bool) -- ^ False: delete symbol
        (Tree Nat) -- ^  height in the precedence


direction qp = case qp of QP dir del ord -> dir
delete    qp = case qp of QP dir del ord -> del
order     qp = case qp of QP dir del ord -> ord

type Preorder s = s -> s -> Comp

compareS :: Tree Nat -> Preorder Symbol
compareS t x y = 
    let qx = get t x ; qy = get t y
    in  case eqNat qx qy of
            True -> GreaterEquals
            False -> case gtNat qx qy of
                 True -> Greater
                 False -> None

type Delete s = s -> Bool

-- | this relies on memoization (else, it is inefficient)

lpoGT :: Delete s -> Preorder s -> [s] -> [s] -> Bool
lpoGT del comp xs ys = case xs of
    [] -> False
    x : xs' -> case del x of
       True -> lpoGT del comp xs' ys
       False -> lpoGE del comp xs' ys || case ys of
          [] -> True
          y : ys' -> case del y of
              True -> lpoGT del comp (x:xs') ys'
              False -> lpoGT del comp xs ys' && case comp x y of
                  Greater -> True
                  GreaterEquals -> lpoGT del comp xs' ys' 
                  None -> False

lpoGE :: Delete s -> Preorder s -> [s] -> [s] -> Bool
lpoGE del comp xs ys = lpoEQ del comp xs ys || lpoGT del comp xs ys

lpoEQ :: Delete s -> Preorder s -> [s] -> [s] -> Bool
lpoEQ del comp xs ys = case xs of
    [] -> case ys of
        [] -> True
        y : ys' -> case del y of
           True -> lpoEQ del comp xs ys'
           False -> False
    x : xs' -> case del x of
        True -> lpoEQ del comp xs' ys
        False -> case ys of
            [] -> False
            y : ys' -> case del y of
                True -> lpoEQ del comp (x:xs') ys'
                False -> isGreaterEquals (comp x y) 
                      && lpoEQ del comp xs' ys'

compareW :: Delete s -> Preorder s -> Preorder [s]
compareW del comp xs ys = 
    case lpoGT del comp xs ys of
        True -> Greater
        False -> case lpoGE del comp xs ys of
           True -> GreaterEquals
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

   






