module CO4.Example.LPOStandalone
where

import Prelude hiding (lex,lookup)
import CO4.PreludeNat

type Map k v    = [(k,v)]

type Symbol     = Nat

data Term       = Var Symbol
                | Node Symbol [Term]
                deriving (Show)

data Order      = Gr | Eq | NGe

type Rule       = (Term,Term)
type Trs        = [Rule]
type Precedence = Map Symbol Nat

constraint :: Trs -> Precedence -> Bool
constraint rules precedence = 
  all (\(lhs,rhs) -> eqOrder (lpo precedence lhs rhs) Gr) rules

lpo :: Precedence -> Term -> Term -> Order
lpo precedence s t = case t of
  Var x -> case eqTerm s t of 
    False -> case varOccurs x s of
                False -> NGe
                True  -> Gr
    True  -> Eq

  Node g ts  -> case s of
    Var _     -> NGe
    Node f ss -> 
      case all (\si -> eqOrder (lpo precedence si t) NGe) ss of
        False -> Gr
        True  -> case ord precedence f g of
                    Gr  -> case all (\ti -> eqOrder (lpo precedence s ti) Gr) ts of
                             False -> NGe
                             True  -> Gr
                    Eq  -> case all (\ti -> eqOrder (lpo precedence s ti) Gr) ts of
                             False -> NGe
                             True  -> lex (lpo precedence) ss ts
                    NGe -> NGe

ord :: Precedence -> Symbol -> Symbol -> Order
ord precedence a b = 
  let pa = lookup eqSymbol a precedence
      pb = lookup eqSymbol b precedence
  in
      ordNat pa pb

ordNat :: Nat -> Nat -> Order
ordNat a b = case eqNat a b of
  True  -> Eq
  False -> case gtNat a b of
    True  -> Gr
    False -> NGe

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

-- * utilities

lookup :: (k -> k -> Bool) -> k -> Map k v -> v
lookup f k map = case map of
  []   -> undefined
  m:ms -> case m of 
    (k',v) -> case f k k' of
      False -> lookup f k ms
      True  -> v

eqTerm :: Term -> Term -> Bool
eqTerm x y = case x of
  Var u     -> case y of { Var v -> eqSymbol u v; _ -> False }
  Node u us -> case y of
    Node v vs -> (eqSymbol u v) &&
                 (eqList eqTerm us vs)
    _         -> False

eqOrder :: Order -> Order -> Bool
eqOrder x y = case x of
  Gr  -> case y of { Gr  -> True; _ -> False }
  Eq  -> case y of { Eq  -> True; _ -> False }
  NGe -> case y of { NGe -> True; _ -> False }

eqSymbol :: Symbol -> Symbol -> Bool
eqSymbol = eqNat

eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList f xs ys = case xs of
  []   -> case ys of []   -> True
                     _    -> False
  u:us -> case ys of []   -> False
                     v:vs -> (f u v) && (eqList f us vs)
