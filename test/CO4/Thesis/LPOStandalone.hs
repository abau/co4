module CO4.Thesis.LPOStandalone
where

import Prelude hiding (lex)
import CO4.Prelude.Nat

data Pair a b   = Pair a b deriving Show
data List a     = Nill | Conss a (List a) deriving Show

data Term       = Var Nat
                | Node Nat (List Term)
                  deriving Show

data Order      = Gr | Eq | NGe

data TRS        = TRS (List Nat)
                      (List (Pair Term Term))

constraint :: TRS -> List Nat -> Bool
constraint trs precedence = case trs of
  TRS symbols rules ->
    and2 (forall rules   (\rule -> ordered rule precedence))
         (forall symbols (\sym  -> exists precedence sym eqNat))

ordered :: Pair Term Term -> List Nat -> Bool
ordered rule precedence = case rule of 
  Pair lhs rhs -> eqOrder (lpo precedence lhs rhs) Gr

lpo :: List Nat -> Term -> Term -> Order
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

ord :: List Nat -> Nat -> Nat -> Order
ord precedence a b = 
  let run ps = case ps of
        Nill        -> undefined
        Conss p ps' -> case eqNat p a of
          True  -> Gr
          False -> case eqNat p b of
            True  -> NGe
            False -> run ps'
  in
    case eqNat a b of
      True  -> Eq
      False -> run precedence

varOccurs :: Nat -> Term -> Bool
varOccurs var term = case term of
  Var var'  -> eqNat var var'
  Node _ ts -> exists' ts (\t -> varOccurs var t)

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

exists :: List a -> a -> (a -> a -> Bool) -> Bool
exists xs y f = exists' xs (\x -> f x y)

exists' :: List a -> (a -> Bool) -> Bool
exists' xs f = case xs of
  Nill -> False
  Conss y ys -> or2 (f y) (exists' ys f)

and2 :: Bool -> Bool -> Bool
and2 x y = case x of
  False -> False
  True -> y

or2 :: Bool -> Bool -> Bool
or2 x y = case x of
  True -> True
  False -> y
