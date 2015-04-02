module CO4.Test.Thesis.LoopStandalone
where

import Prelude hiding (elem)
import CO4.Prelude.Nat

data Pair a b = Pair a b deriving Show
data List a = Nill | Conss a (List a) deriving Show

data Term = Var Nat
          | Node Nat (List Term)
            deriving (Show)

data Unary = Z | S Unary deriving Show

data Step = Step Term 
                 (Pair Term Term)       -- Rule
                 (List Unary)           -- Position
                 (List (Pair Nat Term)) -- Substitution
                 Term
            deriving Show

data LoopingDerivation = LoopingDerivation (List Step)            -- Derivation
                                           (List Unary)           -- Position
                                           (List (Pair Nat Term)) -- Substitution
                         deriving Show

constraint :: List (Pair Term Term) -> LoopingDerivation -> Bool
constraint trs deriv = isLoopingDerivation trs deriv

isLoopingDerivation :: List (Pair Term Term) -> LoopingDerivation -> Bool
isLoopingDerivation trs loopDeriv = case loopDeriv of
  LoopingDerivation deriv pos sub ->
    case deriv of
      Nill -> False
      Conss step _ -> case step of
        Step t0 _ _ _ _ ->
          let last    = deriveTerm trs t0 deriv
              subLast = getSubterm pos last
              t0'     = applySubstitution sub t0
          in
            eqTerm t0' subLast
                  
deriveTerm :: List (Pair Term Term) -> Term -> (List Step) -> Term
deriveTerm trs term deriv = case deriv of
  Nill -> term
  Conss step steps -> case isValidStep trs step of
    False -> undefined
    True  -> case step of
      Step t0 rule pos sub t1 -> case eqTerm term t0 of
        False -> undefined
        True  -> deriveTerm trs t1 steps

isValidStep :: List (Pair Term Term) -> Step -> Bool
isValidStep trs step = case step of
  Step t0 rule pos sub t1 -> 
    and2 (isValidRule trs rule)
         (case rule of
            Pair lhs rhs -> 
              let subT0  = getSubterm pos t0 
                  lhs'   = applySubstitution sub lhs
                  rhs'   = applySubstitution sub rhs
                  result = putSubterm t0 pos rhs'
              in
                and2 (eqTerm subT0 lhs')
                     (eqTerm result t1)
         )

isValidRule :: List (Pair Term Term) -> Pair Term Term -> Bool
isValidRule trs rule = elem eqRule rule trs

getSubterm :: (List Unary) -> Term -> Term
getSubterm pos term = case pos of
  Nill -> term
  Conss p pos' -> case term of
    Var v -> undefined
    Node f ts -> getSubterm pos' (at p ts)

putSubterm :: Term -> (List Unary) -> Term -> Term
putSubterm term pos term' = case pos of
  Nill -> term'
  Conss p pos' -> case term of
    Var v -> undefined
    Node f ts -> Node f (replace p ts (putSubterm (at p ts) pos' term'))

applySubstitution :: List (Pair Nat Term) -> Term -> Term
applySubstitution subs term = case term of
  Var v -> applySubstitutionToVar subs v
  Node f ts -> Node f (map' (\t -> applySubstitution subs t) ts)

applySubstitutionToVar :: List (Pair Nat Term) -> Nat -> Term
applySubstitutionToVar sub v = case sub of
  Nill -> Var v -- ??
  Conss s ss -> case s of 
    Pair name term -> case eqNat v name of 
      False -> applySubstitutionToVar ss v
      True  -> term

at :: Unary -> List a -> a
at u xs = case xs of
  Nill -> undefined
  Conss y ys -> case u of
    Z -> y
    S u' -> at u' ys

replace :: Unary -> List a -> a -> List a
replace u xs x = case xs of
  Nill -> undefined
  Conss y ys -> case u of
    Z -> Conss x ys
    S u' -> Conss y (replace u' ys x)


eqRule :: Pair Term Term -> Pair Term Term -> Bool
eqRule x y = case x of
  Pair xLhs xRhs -> case y of 
    Pair yLhs yRhs -> and2 (eqTerm xLhs yLhs) (eqTerm xRhs yRhs)

elem :: (a -> a -> Bool) -> a -> List a -> Bool
elem eq x xs = case xs of
  Nill -> False
  Conss y ys -> or2 (eq x y) (elem eq x ys)

eqTerm :: Term -> Term -> Bool
eqTerm x y = case x of
  Var xV -> case y of
    Var yV -> eqNat xV yV
    Node g ts -> False
  Node f ss -> case y of
    Var yV -> False
    Node g ts -> and2 (eqNat f g) (eqList eqTerm ss ts)

eqList :: (a -> a -> Bool) -> List a -> List a -> Bool
eqList eq xs ys = case xs of
  Nill -> case ys of 
    Nill -> True
    Conss y ys' -> False
  Conss x xs' -> case ys of 
    Nill -> False
    Conss y ys' -> and2 (eq x y) (eqList eq xs' ys')

or2 :: Bool -> Bool -> Bool
or2 x y = case x of
  True  -> True
  False -> y

and2 :: Bool -> Bool -> Bool
and2 x y = case x of
  False -> False
  True  -> y

map' :: (a -> b) -> List a -> List b
map' f xs = case xs of
  Nill -> Nill
  Conss y ys -> Conss (f y) (map' f ys)
