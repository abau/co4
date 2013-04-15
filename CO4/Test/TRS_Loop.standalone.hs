module TRS_Loop where

import qualified Prelude

main ld = looping_derivation_ok nonTermF ld

-- http://termcomp.uibk.ac.at/termcomp/tpdb/tpviewer.seam?tpId=54227&cid=3363
data Term = V Name | Ap Term Term | F | C | N | Foldr

data Name = X | XS | G | H 

data List a = Nil | Cons a (List a)

data Rule = Rule ( List Name ) -- variables
                 Term -- lhs
                 Term -- rhs

type TRS = List Rule
nonTermF :: TRS
nonTermF = 
    Cons ( Rule (Cons X Nil)
                ( Ap(Ap F (V X)) (V X) )
                ( Ap(Ap(V X)(Ap(F)(V X)))(Ap(Ap(C)(V X))(N))) )
   (Cons (Rule (Cons G (Cons H Nil))
               (Ap(Ap(Ap(Foldr)(V G))(V H))(N) )
               (V H))
   (Cons (Rule (Cons G (Cons H (Cons X (Cons XS Nil))))
               (Ap(Ap(Ap(Foldr)(V G))(V H))(Ap(Ap(C)(V X))(V XS)))
               (Ap(Ap(V G)(V X))(Ap(Ap(Ap(Foldr)(V G))(V H))(V XS))))
         Nil))

data Bool = False | True
type Position = List Bool -- False = links, ..

data Pair a b = Pair a b
type Substitution = List (Pair Name Term)

data Step = Step Term Rule Position Substitution Term

-- substitution auf term anwenden.
-- dabei soll JEDE variable im term auch
-- ersetzt werden 
apply :: Term -> Substitution -> Term
apply = Prelude.undefined

data Maybe a = Nothing | Just a

get :: Term -> Position -> Term
get = Prelude.undefined

put :: Term -> Position -> Term -> Term
put = Prelude.undefined

step_ok :: TRS -> Step -> Bool
step_ok trs s = case s of
  Step t0 rule pos sub t1 -> 
    and2 (elemRule rule trs) 
     (case rule of
        Rule vars lhs rhs -> 
            -- domain sub == vars  ??
         and2 
           ( equalTerm (get t0 pos) (apply lhs sub))
           (equalTerm (put t0 pos (apply rhs sub)) t1))

type Derivation = List Step

derivation_ok :: TRS -> Derivation -> Bool
derivation_ok = Prelude.undefined

data Looping_Derivation = 
   Looping_Derivation Derivation Position Substitution
looping_derivation_ok trs ld = case ld of
    Looping_Derivation der pos sub ->
        and2 (derivation_ok trs der)
            (equalTerm (get (last der) pos)
                       (apply (head der) sub))


equalTerm :: Term -> Term -> Bool
equalTerm = Prelude.undefined

elemRule :: Rule -> List Rule -> Bool
elemRule = Prelude.undefined







or2 x y = case x of
    False -> y
    True  -> x

and2 x y = case x of
    False -> x
    True  -> y

not x  = case x of
    False -> True
    True -> False




null xs = case xs of
    Nil -> True
    Cons x xs -> False

head xs = case xs of
    Nil -> undefined
    Cons x xs -> x

last xs = case xs of
    Nil -> undefined
    Cons x xs -> case xs of
        Nil -> x
        Cons x ys -> last xs

{-

eqListSigma xs ys = case xs of
    Nil -> case ys of
        Nil -> True
        Cons y ys -> False
    Cons x xs -> case ys of
        Nil -> False
        Cons y ys -> 
            and2 (eqSigma x y) (eqListSigma xs ys)



foldr f z xs = case xs of
    Nil -> z
    Cons x xs -> f x (foldr f z xs)

map f xs = foldr ( \ x y -> Cons (f x) y ) Nil xs

or  xs = foldr or2 False xs
and xs = foldr and2 True xs

forall xs f = and ( map f xs )
exists xs f = or  ( map f xs )



eqRule u1 u2 = case u1 of
        Rule l1 r1 -> case u2 of
            Rule l2 r2 -> 
                and2 (eqListSigma l1 l2)
                     (eqListSigma r1 r2)

data RS = RS (List Rule)

data Step = Step (List Sigma) -- prefix
                 Rule
                 (List Sigma) -- suffix

-- type Derivation = List Step

looping_derivation rs d =
   and2  True -- (break_symmetry d)
    ( and2 (derivation_is_nonempty d)
     ( and2 (derivation_uses_rules rs d)
      (and2 (derivation_is_joinable d)
           (derivation_is_looping d))))

break_symmetry d = case d of
    Nil -> False
    Cons s later -> case s of
        Step p u s -> null p

derivation_uses_rules rs d = case rs of
    RS rules -> forall d
        ( \ s -> step_uses_rules rules s )

derivation_is_nonempty d = not (null d)

derivation_is_joinable d = case d of
    Nil -> True
    Cons s1 later1 -> case later1 of
        Nil -> True
        Cons s2 later2 -> 
            and2 (joinable_steps s1 s2)
                 (derivation_is_joinable later1)

-- TODO: this is what I want to write:
-- right_semantics (Step p (Rule l r) s) = 
--    append p (append r s)

left_semantics step = case step of
    Step p u s -> case u of
        Rule l r -> append p (append l s)

right_semantics step = case step of
    Step p u s -> case u of
        Rule l r -> append p (append r s)

derivation_is_looping d = 
      -- factor (left_semantics (head d)) (right_semantics (last d))
      suffix (left_semantics (head d)) (right_semantics (last d))

joinable_steps step1 step2 = 
    -- eqListSigma (right_semantics step1) (left_semantics  step2)
    -- factor (left_semantics  step2) (right_semantics step1) 
    prefix (left_semantics  step2) (right_semantics step1) 

step_uses_rules rules step = case step of
    Step p u s -> 
        exists rules ( \ v -> eqRule u v )
-}
