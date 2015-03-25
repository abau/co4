module CO4.Test.Thesis.LPOStandalone
where

import Prelude hiding (lookup,lex)
import CO4.Prelude.Nat

data Pair a b   = Pair a b deriving Show
data List a     = Nill | Conss a (List a) deriving Show

data Term       = Var Nat
                | Node Nat (List Term)
                  deriving Show

data Order      = Gr | Eq | NGe

constraint :: List (Pair Term Term) -> List (Pair Nat Nat) -> Bool
constraint rules precedence = 
  forall rules ( \rule -> case rule of 
                   Pair lhs rhs -> eqOrder (lpo precedence lhs rhs) Gr )

lpo :: List (Pair Nat Nat) -> Term -> Term -> Order
lpo precedence s t = case t of
  Var x -> case eqTerm s t of 
    False -> case varOccurs x s of
                False -> NGe
                True  -> Gr
    True  -> Eq

  Node g ts  -> case s of
    Var _     -> NGe
    Node f ss -> 
      case forall ss (\si -> eqOrder (lpo precedence si t) NGe) of
        False -> Gr
        True  -> case ord precedence f g of
          Gr  -> case forall ts (\ti -> eqOrder (lpo precedence s ti) Gr) of
                   False -> NGe
                   True  -> Gr
          Eq  -> case forall ts (\ti -> eqOrder (lpo precedence s ti) Gr) of
                   False -> NGe
                   True  -> lex (lpo precedence) ss ts
          NGe -> NGe

ord :: List (Pair Nat Nat) -> Nat -> Nat -> Order
ord precedence a b = 
  let pa = lookup eqNat a precedence
      pb = lookup eqNat b precedence
  in
      ordNat pa pb

ordNat :: Nat -> Nat -> Order
ordNat a b = case eqNat a b of
  True  -> Eq
  False -> case gtNat a b of
    True  -> Gr
    False -> NGe

varOccurs :: Nat -> Term -> Bool
varOccurs var term = case term of
  Var var'  -> eqNat var var'
  Node _ ts -> exists ts (\t -> varOccurs var t)

lex :: (a -> b -> Order) -> List a -> List b -> Order
lex ord xs ys = case xs of
  Nill -> case ys of Nill -> Eq
                     Conss y ys' -> NGe
  Conss x xs' -> case ys of 
    Nill -> Gr
    Conss y ys' -> case ord x y of 
      Gr  -> Gr
      Eq  -> lex ord xs' ys'
      NGe -> NGe

-- * utilities

lookup :: (k -> k -> Bool) -> k -> List (Pair k v) -> v
lookup f k map = case map of
  Nill -> undefined
  Conss m ms -> case m of 
    Pair k' v -> case f k k' of
      False -> lookup f k ms
      True  -> v

eqTerm :: Term -> Term -> Bool
eqTerm x y = case x of
  Var u -> case y of 
    Var v -> eqNat u v
    Node v vs -> False

  Node u us -> case y of
    Var v -> False
    Node v vs -> and2 (eqNat u v) (eqList eqTerm us vs)

eqOrder :: Order -> Order -> Bool
eqOrder x y = case x of
  Gr  -> case y of Gr  -> True
                   Eq  -> False
                   NGe -> False
  Eq  -> case y of Gr  -> False
                   Eq  -> True
                   NGe -> False
  NGe -> case y of Gr  -> False
                   Eq  -> False
                   NGe -> True

eqList :: (a -> a -> Bool) -> List a -> List a -> Bool
eqList f xs ys = case xs of
  Nill -> case ys of Nill -> True
                     Conss v vs -> False
  Conss u us -> case ys of Nill -> False
                           Conss v vs -> and2 (f u v) (eqList f us vs)

forall :: List a -> (a -> Bool) -> Bool
forall xs f = case xs of
  Nill -> True
  Conss y ys -> and2 (f y) (forall ys f)

exists :: List a -> (a -> Bool) -> Bool
exists xs f = case xs of
  Nill -> True
  Conss y ys -> or2 (f y) (exists ys f)

and2 :: Bool -> Bool -> Bool
and2 x y = case x of
  False -> False
  True -> y

or2 :: Bool -> Bool -> Bool
or2 x y = case x of
  True -> True
  False -> y
