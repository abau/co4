module TermComp where

import qualified Prelude

data Bool     = False | True

data List a   = Nil | Cons a (List a)

type Symbol   = List Bool
type Variable = List Bool

data Term = Var Variable
          | Term Symbol (List Term)

data Order = GR | EQ | NGE

data Pair a b = Pair a b

type Precedence = List Symbol

type TRS = List (Pair Term Term)

main trs prec = 
  and2 (isPrec prec)
       (all (\rule -> case rule of
                          Pair lhs rhs -> equalOrder (lpo (ord prec) lhs rhs) GR
                      )
                      trs
       )

isPrec prec = case prec of
  Nil -> True
  Cons p ps -> and2 (not (elem equalSymbol p ps))
                    (isPrec ps)

lpo :: (Symbol -> Symbol -> Order) -> Term -> Term -> Order
lpo ord s t = case t of
  Var x     -> case equalTerm s t of 
                  False -> case occurs x s of
                              False -> NGE
                              True  -> GR
                  True  -> EQ

  Term g ts  -> case s of
    Var _     -> NGE
    Term f ss -> 
      case all (\si -> equalOrder (lpo ord si t) NGE) ss of
        False -> GR
        True  -> case ord f g of
                    GR  -> case all (\ti -> equalOrder (lpo ord s ti) GR) ts of
                             False -> NGE
                             True  -> GR
                    EQ  -> case all (\ti -> equalOrder (lpo ord s ti) GR) ts of
                             False -> NGE
                             True  -> lex (lpo ord) ss ts
                    NGE -> NGE

ord :: Precedence -> Symbol -> Symbol -> Order
ord prec a b = case equalSymbol a b of
  False -> case greater prec a b of
              False -> NGE
              True  -> GR
  True  -> EQ

greater :: Precedence -> Symbol -> Symbol -> Bool
greater prec a b = case prec of
  Nil          -> False
  Cons p prec' -> or2 (and2 (equalSymbol p a)
                            (elem equalSymbol b prec')
                      )
                      (greater prec' a b)
  
  {-case equalSymbol p a of 
    False -> case equalSymbol p b of
      False -> greater prec' a b
      True  -> False
    True  -> True
    -}

occurs :: Variable -> Term -> Bool
occurs v term = case term of
  Var v'    -> equalVariable v v'
  Term _ ts -> any (occurs v) ts

lex :: (a -> b -> Order) -> List a -> List b -> Order
lex ord xs ys = case xs of
  Nil -> case ys of 
    Nil      -> EQ
    Cons _ _ -> NGE

  Cons x xs' -> case ys of 
    Nil        -> GR
    Cons y ys' -> case ord x y of 
      GR  -> GR
      EQ  -> lex ord xs' ys'
      NGE -> NGE

equalSymbol :: Symbol -> Symbol -> Bool
equalSymbol a b = equalList equalBool a b

equalVariable :: Variable -> Variable -> Bool
equalVariable a b = equalList equalBool a b

equalTerm :: Term -> Term -> Bool
equalTerm s t = case s of
  Var x -> case t of Var y    -> equalVariable x y
                     Term _ _ -> False
  Term x xs -> 
    case t of Var _     -> False
              Term y ys -> and2 (equalSymbol x y)
                                (equalList equalTerm xs ys)

equalOrder :: Order -> Order -> Bool
equalOrder a b = case a of
  GR  -> case b of GR  -> True
                   EQ  -> False
                   NGE -> False
  EQ  -> case b of GR  -> False
                   EQ  -> True
                   NGE -> False
  NGE -> case b of GR  -> False
                   EQ  -> False
                   NGE -> True

equalList :: (a -> a -> Bool) -> List a -> List a -> Bool
equalList eq xs ys = case xs of
  Nil -> case ys of Nil -> True
                    _   -> False
  Cons x' xs' -> 
    case ys of Nil         -> False
               Cons y' ys' -> and2 (eq x' y') (equalList eq xs' ys')

equalBool :: Bool -> Bool -> Bool
equalBool x y = case x of
  False -> not y
  True  -> y
    
elem :: (a -> a -> Bool) -> a -> List a -> Bool
elem eq x xs = case xs of
  Nil       -> False
  Cons y ys -> or2 (eq x y) (elem eq x ys)

all :: (a -> Bool) -> List a -> Bool
all f xs = and (map f xs)

any :: (a -> Bool) -> List a -> Bool
any f xs = or (map f xs)

and :: List Bool -> Bool
and xs = foldr and2 True xs

or :: List Bool -> Bool
or xs = foldr or2 False xs

foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z xs = case xs of
    Nil -> z
    Cons x xs -> f x (foldr f z xs)

map :: (a -> b) -> List a -> List b
map f xs = foldr ( \ x y -> Cons (f x) y ) Nil xs

or2 :: Bool -> Bool -> Bool
or2 x y = case x of
    False -> y
    True  -> x

and2 :: Bool -> Bool -> Bool
and2 x y = case x of
    False -> x
    True  -> y

not :: Bool -> Bool 
not x  = case x of
    False -> True
    True -> False

