module CO4.Thesis.LPOSLStandalone
where

import Prelude (Bool(..),undefined)
import CO4.Prelude.Nat

data Pair a b         = Pair a b
data Triple a b c     = Triple a b c
data List a           = Nill | Conss a (List a)

type Symbol           = Nat
type Map k v          = List (Pair k v)
type Function         = Map (List Nat) Nat
type Interpretation a = Map a Function
type Sigma            = Map Symbol Nat
type Label            = List Nat
type Labelled a       = Pair a Label
type Precedence a     = List a

data Term a           = Var Symbol | Node a (List (Term a))
type Rule a           = Pair (Term a) (Term a)
type TRS a            = Pair (List a) (List (Rule a))

data Order            = Gr | Eq | NGe

constraint :: Triple (TRS Symbol) (List (Labelled Symbol)) (List Sigma) 
           -> Pair (Precedence (Labelled Symbol)) (Interpretation Symbol)
           -> Bool
constraint p u = 
  let eqSymbol         = eqNat
      eqLabelledSymbol = eqLabelled eqNat
  in
    case p of 
      Triple trs lsymbols assignments ->
        case u of 
          Pair precedence interpretation ->
            case trs of
              Pair _ rules ->
                let lrules = labelledRules eqNat interpretation assignments rules
                    ltrs   = Pair lsymbols lrules
                in
                  and2 (lpoConstraint eqLabelledSymbol ltrs precedence)
                       (isModel eqNat interpretation assignments trs)

-- * LPO

lpoConstraint :: (a -> a -> Bool) -> TRS a -> Precedence a -> Bool
lpoConstraint eq trs precedence = case trs of 
  Pair symbols rules -> and2 (forall rules   (\rule -> ordered eq rule precedence))
                             (forall symbols (\sym  -> exists precedence sym eq))

ordered :: (a -> a -> Bool) -> Rule a -> Precedence a -> Bool
ordered eq rule precedence = case rule of
  Pair lhs rhs -> eqOrder (lpo eq precedence lhs rhs) Gr

lpo :: (a -> a -> Bool) -> Precedence a -> Term a -> Term a -> Order
lpo eq precedence s t = case t of
  Var x -> case eqTerm eq s t of 
    False -> case varOccurs x s of
                False -> NGe
                True  -> Gr
    True  -> Eq

  Node g ts  -> case s of
    Var _     -> NGe
    Node f ss -> 
      case forall ss (\si -> eqOrder (lpo eq precedence si t) NGe) of
        False -> Gr
        True  -> case ord eq precedence f g of
          Gr  -> case forall ts (\ti -> eqOrder (lpo eq precedence s ti) Gr) of
                   False -> NGe
                   True  -> Gr
          Eq  -> case forall ts (\ti -> eqOrder (lpo eq precedence s ti) Gr) of
                   False -> NGe
                   True  -> lex (lpo eq precedence) ss ts
          NGe -> NGe

ord :: (a -> a -> Bool) -> Precedence a -> a -> a -> Order
ord eq precedence a b = 
  let run ps = case ps of
        Nill        -> undefined
        Conss p ps' -> case eq p a of
          True  -> Gr
          False -> case eq p b of
            True  -> NGe
            False -> run ps'
  in
    case eq a b of
      True  -> Eq
      False -> run precedence

varOccurs :: Symbol -> Term a -> Bool
varOccurs var term = case term of
  Var var' -> eqNat var var'
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

-- * Semantic Labelling

labelledRules :: (a -> a -> Bool) -> Interpretation a -> List Sigma
              -> List (Rule a) -> List (Rule (Labelled a))
labelledRules eq interpretation assignments rules =
    concat' (map' (\rule -> case rule of
      Pair lhs rhs ->
        map' (\sigma -> Pair (labelledTerm eq interpretation sigma lhs)
                             (labelledTerm eq interpretation sigma rhs)
             ) assignments) rules)

labelledTerm :: (a -> a -> Bool) -> Interpretation a -> Sigma -> Term a -> Term (Labelled a)
labelledTerm eq interpretation sigma t = case t of
  Var v     -> Var v
  Node f ts -> let as  = map' (eval eq interpretation sigma) ts
                   ts' = map' (labelledTerm eq interpretation sigma) ts
               in
                 Node (Pair f as) ts'

isModel :: (a -> a -> Bool) -> Interpretation a -> List Sigma -> TRS a -> Bool
isModel eq interpretation assignments trs = case trs of
  Pair _ rules ->
    forall assignments (\sigma -> 
      forall rules (\(Pair lhs rhs) -> eqNat (eval eq interpretation sigma lhs)
                                             (eval eq interpretation sigma rhs)))

eval :: (a -> a -> Bool) -> Interpretation a -> Sigma -> Term a -> Nat
eval eq interpretation sigma t = 
  let lookup f k map = case map of
        Nill -> undefined
        Conss m ms -> case m of 
          Pair k' v -> case f k k' of
            False -> lookup f k ms
            True  -> v
  in case t of
    Var  v    -> lookup eqNat v sigma
    Node f ts -> let i  = lookup eq f interpretation
                     as = map' (\t -> eval eq interpretation sigma t) ts
                 in
                   lookup (eqList eqNat) as i

-- * Utilities

eqTerm :: (a -> a -> Bool) -> Term a -> Term a -> Bool
eqTerm eq x y = case x of
  Var u -> case y of 
    Var v -> eqNat u v
    Node v vs -> False

  Node u us -> case y of
    Var v -> False
    Node v vs -> and2 (eq u v) (eqList (eqTerm eq) us vs)

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

eqLabelled :: (a -> a -> Bool) -> Labelled a -> Labelled a -> Bool
eqLabelled eq (Pair a aLabel) (Pair b bLabel) =
  and2 (eq a b) (eqList eqNat aLabel bLabel)

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

concat' :: List (List a) -> List a
concat' xs = foldr' append' Nill xs

append' :: List a -> List a -> List a
append' a b = foldr' Conss b a

foldr' :: (a -> b -> b) -> b -> List a -> b
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
