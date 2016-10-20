module CO4.Thesis.LPOSLStandalone
where

import Prelude (Bool(..),undefined)
import CO4.Prelude.Nat

data Pair a b       = Pair a b
data Triple a b c   = Triple a b c
data List a         = Nill | Conss a (List a)

type Symbol         = Nat
type Map k v        = List (Pair k v)
type Function       = Map (List Nat) Nat
type Interpretation = Map Symbol Function
type Sigma          = Map Symbol Nat
type Label          = List Nat
type LSymbol        = Pair Symbol Label
type Precedence     = List LSymbol

data Term           = Var Symbol
                    | Node Symbol (List Term)

data LTerm          = LVar Symbol
                    | LNode LSymbol (List LTerm)

data Order          = Gr | Eq | NGe

type Rule           = Pair Term Term
type LRule          = Pair LTerm LTerm
type TRS            = Pair (List Symbol) (List Rule)
type LTRS           = Pair (List LSymbol) (List LRule)

constraint :: Triple TRS (List LSymbol) (List Sigma) -> Pair Precedence Interpretation -> Bool
constraint p u = 
  case p of 
    Triple trs lsymbols assignments ->
      case u of 
        Pair precedence interpretation ->
          case trs of
            Pair _ rules ->
              let ltrs = Pair lsymbols (labelledRules interpretation assignments rules)
              in
                and2 (lpoConstraint ltrs precedence)
                     (isModel interpretation assignments trs)

-- * lpo

lpoConstraint :: LTRS -> Precedence -> Bool
lpoConstraint ltrs precedence =
  case ltrs of 
    Pair symbols rules -> and2 (forall rules   (\rule -> ordered rule precedence))
                               (forall symbols (\sym  -> exists precedence sym eqLSymbol))

ordered :: LRule -> Precedence -> Bool
ordered lrule precedence = 
  case lrule of
    Pair lhs rhs -> eqOrder (lpo precedence lhs rhs) Gr

lpo :: Precedence -> LTerm -> LTerm -> Order
lpo precedence s t = case t of
  LVar x -> case eqLTerm s t of 
    False -> case varOccurs x s of
                False -> NGe
                True  -> Gr
    True  -> Eq

  LNode g ts  -> case s of
    LVar _     -> NGe
    LNode f ss -> 
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

ord :: Precedence -> LSymbol -> LSymbol -> Order
ord precedence a b = 
  let run ps = case ps of
        Nill        -> undefined
        Conss p ps' -> case eqLSymbol p a of
          True  -> Gr
          False -> case eqLSymbol p b of
            True  -> NGe
            False -> run ps'
  in
    case eqLSymbol a b of
      True  -> Eq
      False -> run precedence

varOccurs :: Symbol -> LTerm -> Bool
varOccurs var term = case term of
  LVar var' -> eqNat var var'
  LNode _ ts -> exists' ts (\t -> varOccurs var t)

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

-- * semantic labelling

labelledRules :: Interpretation -> List Sigma -> List Rule -> List LRule
labelledRules interpretation assignments rules =
    concat' (map' (\rule -> case rule of
      Pair lhs rhs ->
        map' (\sigma -> Pair (labelledTerm interpretation sigma lhs)
                             (labelledTerm interpretation sigma rhs)
             ) assignments) rules)

labelledTerm :: Interpretation -> Sigma -> Term -> LTerm
labelledTerm interpretation sigma t = case t of
  Var v     -> LVar v
  Node f ts -> let as  = map' (eval interpretation sigma) ts
                   ts' = map' (labelledTerm interpretation sigma) ts
               in
                 LNode (Pair f as) ts'

isModel :: Interpretation -> List Sigma -> TRS -> Bool
isModel interpretation assignments trs = case trs of
  Pair _ rules ->
    forall assignments (\sigma -> 
      forall rules (\(Pair lhs rhs) -> eqNat (eval interpretation sigma lhs)
                                             (eval interpretation sigma rhs)))

eval :: Interpretation -> Sigma -> Term -> Nat
eval interpretation sigma t = 
  let lookup f k map = case map of
        Nill -> undefined
        Conss m ms -> case m of 
          Pair k' v -> case f k k' of
            False -> lookup f k ms
            True  -> v
  in case t of
    Var  v    -> lookup eqNat v sigma
    Node f ts -> let i  = lookup eqNat f interpretation
                     as = map' (eval interpretation sigma) ts
                 in
                   lookup (eqList eqNat) as i

-- * utilities

eqLTerm :: LTerm -> LTerm -> Bool
eqLTerm x y = case x of
  LVar u -> case y of 
    LVar v -> eqNat u v
    LNode v vs -> False

  LNode u us -> case y of
    LVar v -> False
    LNode v vs -> and2 (eqLSymbol u v) (eqList eqLTerm us vs)

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

eqLSymbol :: LSymbol -> LSymbol -> Bool
eqLSymbol (Pair xSym xLabel) (Pair ySym yLabel) =
  and2 (eqNat xSym ySym) (eqList eqNat xLabel yLabel)

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

map' :: (a -> b) -> List a -> List b
map' f xs = case xs of
  Nill -> Nill
  Conss y ys -> Conss (f y) (map' f ys)

concat' xs = foldr' append' Nill xs
append' a b = foldr' Conss b a
foldr' n c xs = case xs of 
  Nill -> c
  Conss y ys -> n y (foldr' n c ys)

and2 :: Bool -> Bool -> Bool
and2 x y = case x of
  False -> False
  True -> y

or2 :: Bool -> Bool -> Bool
or2 x y = case x of
  True -> True
  False -> y
