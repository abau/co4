module CO4.Test.TermComp2014.Standalone.LPO
where

import Prelude hiding (lex,lookup)

-- Keep in sync with CO4.Test.TermComp2014.Data
type Map k v = [(k,v)]

type Symbol  = [Bool]

data Term = Var  Symbol
          | Node Symbol [Term]

data Rule = Rule Term Term

data Trs  = Trs [Rule]

data Order = Gr | Eq | NGe

data Nat   = Zero | Succ Nat

type Precedence = Map Symbol Nat

constraint :: Trs -> Precedence -> Bool
constraint (Trs rules) precedence = 
  all (\(Rule lhs rhs) -> eqOrder (lpo (ord precedence) lhs rhs) Gr) rules

lpo :: (Symbol -> Symbol -> Order) -> Term -> Term -> Order
lpo ord s t = case t of
  Var x -> case eqTerm s t of 
    False -> case varOccurs x s of
                False -> NGe
                True  -> Gr
    True  -> Eq

  Node g ts  -> case s of
    Var _     -> NGe
    Node f ss -> 
      case all (\si -> eqOrder (lpo ord si t) NGe) ss of
        False -> Gr
        True  -> case ord f g of
                    Gr  -> case all (\ti -> eqOrder (lpo ord s ti) Gr) ts of
                             False -> NGe
                             True  -> Gr
                    Eq  -> case all (\ti -> eqOrder (lpo ord s ti) Gr) ts of
                             False -> NGe
                             True  -> lex (lpo ord) ss ts
                    NGe -> NGe

ord :: Precedence -> Symbol -> Symbol -> Order
ord prec a b = 
  let pa = lookup eqSymbol a prec
      pb = lookup eqSymbol b prec
  in
    ordNat pa pb

ordNat :: Nat -> Nat -> Order
ordNat a b = case a of
  Zero    -> case b of Zero    -> Eq
                       _       -> NGe
  Succ a' -> case b of Zero    -> Gr
                       Succ b' -> ordNat a' b'

varOccurs :: Symbol -> Term -> Bool
varOccurs var term = case term of
  Var var'  -> eqSymbol var var'
  Node _ ts -> any (varOccurs var) ts

lex :: (a -> b -> Order) -> [a] -> [b] -> Order
lex ord xs ys = case xs of
  [] -> case ys of [] -> Eq
                   _  -> NGe
  x:xs' -> case ys of 
    []    -> Gr
    y:ys' -> case ord x y of 
      Gr  -> Gr
      Eq  -> lex ord xs' ys'
      NGe -> NGe

eqTerm :: Term -> Term -> Bool
eqTerm x y = case x of
  Var u     -> case y of { Var v -> eqSymbol u v; _ -> False }
  Node u us -> case y of
    Node v vs -> (eqSymbol u v) && (eqList eqTerm us vs)
    _         -> False

lookup :: (k -> k -> Bool) -> k -> Map k v -> v
lookup f k map = case map of
  []   -> undefined
  m:ms -> case m of 
    (k',v) -> case f k k' of
      False -> lookup f k ms
      True  -> v

eqOrder :: Order -> Order -> Bool
eqOrder x y = case x of
  Gr  -> case y of { Gr  -> True; _ -> False }
  Eq  -> case y of { Eq  -> True; _ -> False }
  NGe -> case y of { NGe -> True; _ -> False }

eqSymbol :: Symbol -> Symbol -> Bool
eqSymbol = eqList eqBool

eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList f xs ys = case xs of
  []   -> case ys of []   -> True
                     _    -> False
  u:us -> case ys of []   -> False
                     v:vs -> (f u v) && (eqList f us vs)

eqBool :: Bool -> Bool -> Bool
eqBool x y = case x of
  False -> not y
  True  -> y
