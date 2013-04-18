module TRS_Loop where

import qualified Prelude
import           Prelude (undefined)

main :: Looping_Derivation -> Bool
--main ld = looping_derivation_ok toyama ld
--main ld = looping_derivation_ok nonTermF ld
main ld = looping_derivation_ok simple ld

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

toyamaRule1 =
  let x = X; f = F; a = C; b = N
  in
    Rule (Cons x Nil) (Ap (Ap (Ap f a) b) (V x)) 
                      (Ap (Ap (Ap f (V x)) (V x)) (V x))

toyamaRule2 = 
  let x = X; y = XS; g = Foldr
  in
   Rule (Cons x (Cons y Nil)) (Ap (Ap g (V x)) (V y)) (V x)

toyamaRule3 = 
  let x = X; y = XS; g = Foldr
  in
   Rule (Cons x (Cons y Nil)) (Ap (Ap g (V x)) (V y)) (V y)

toyama :: TRS
toyama = Cons toyamaRule1 (Cons toyamaRule2 (Cons toyamaRule3 Nil))
		
simpleRule1 = Rule (Cons X Nil) (Ap F (V X)) (Ap C (V X))
simpleRule2 = Rule (Cons X Nil) (Ap C (V X)) (Ap F (V X))

simple :: TRS
simple = Cons simpleRule1 (Cons simpleRule2 Nil)

simpleSolution :: Looping_Derivation
simpleSolution = 
  let step1 = Step (Ap F (V X)) simpleRule1 Nil Nil (Ap C (V X))
      step2 = Step (Ap C (V X)) simpleRule2 Nil Nil (Ap F (V X))
  in
    Looping_Derivation (Cons step1 (Cons step2 Nil))
                       Nil
                       Nil

noSimpleSolution = Looping_Derivation 
  (Cons (Step (Ap C (V X)) (Rule (Cons X Nil) (Ap C (V X)) (Ap F (V X))) Nil Nil (Ap F (V X)))
  (Cons (Step N (Rule Nil N (V X)) Nil Nil (Ap C (V X))) Nil)) 
  Nil Nil

data Bool = False | True
  --deriving Prelude.Show
type Position = List Bool -- False = links, ..

data Pair a b = Pair a b

fst :: Pair a b -> a
fst p = case p of Pair a _ -> a

snd :: Pair a b -> b
snd p = case p of Pair _ b -> b

type Substitution = List (Pair Name Term)

domain :: Substitution -> List Name
domain = map fst

--data Step = Step Term Rule Position Substitution Term
data Step = Step Term Rule (List Bool) (List (Pair Name Term)) Term

stepInput :: Step -> Term
stepInput step = case step of Step t _ _ _ _ -> t

stepResult :: Step -> Term
stepResult step = case step of Step _ _ _ _ t -> t

-- substitution auf term anwenden.
-- dabei soll JEDE variable im term auch
-- ersetzt werden 
apply :: Term -> Substitution -> Term
apply = foldr apply'
  where
    apply' pair term = case pair of
      Pair name term' -> replaceName term name term'

replaceName :: Term -> Name -> Term -> Term
replaceName term name term' = case term of
  V n    -> case equalName n name of
              False -> term
              True  -> term'
  Ap a b -> Ap (replaceName a name term') (replaceName b name term')
  F      -> F
  C      -> C
  N      -> N
  Foldr  -> Foldr

data Maybe a = Nothing | Just a

get :: Term -> Position -> Term
get term pos = case pos of
  Nil -> term
  Cons p pos' -> case term of
    V _     -> undefined
    Ap a b -> case p of False -> get a pos'
                        True  -> get b pos'
    F       -> undefined
    C       -> undefined
    N       -> undefined
    Foldr   -> undefined

put :: Term -> Position -> Term -> Term
put term pos term' = case pos of
  Nil -> term'
  Cons p pos' -> 
    case term of
      V _     -> undefined
      Ap a b -> case p of False -> Ap (put a pos' term') b
                          True  -> Ap a (put b pos' term')
      F       -> undefined
      C       -> undefined
      N       -> undefined
      Foldr   -> undefined

rule_ok :: TRS -> Rule -> Bool
rule_ok trs rule = elem equalRule rule trs
  
step_ok :: TRS -> Step -> Bool
step_ok trs s = case s of
  Step t0 rule pos sub t1 -> 
    and2 (rule_ok trs rule)
     (case rule of
        Rule vars lhs rhs -> 
         and ( -- Cons (equalList equalName (domain sub) vars)
             (Cons (equalTerm (get t0 pos) (apply lhs sub))
             (Cons (equalTerm (put t0 pos  (apply rhs sub)) t1)
             Nil))))

type Derivation = List Step

derivation_ok :: TRS -> Derivation -> Bool
derivation_ok trs steps = and2 ( snd ( foldr derive_ok (Pair Nothing True) steps ) )
                               ( not ( null steps ) )
  where 
    derive_ok step state = case state of
      Pair previous isOk ->
        case previous of 
          Nothing -> Pair (Just step)       (step_ok trs step)
          Just p  -> Pair (Just step) (and (Cons (step_ok trs step)
                                           (Cons (match p step)
                                           (Cons isOk Nil)))
                                      )

    match a b = equalTerm (stepResult a) (stepInput b)


--data Looping_Derivation = Looping_Derivation Derivation Position Substitution
data Looping_Derivation = Looping_Derivation (List Step) 
                                             (List Bool) 
                                             (List (Pair Name Term))

looping_derivation_ok :: TRS -> Looping_Derivation -> Bool
looping_derivation_ok trs ld = case ld of
    Looping_Derivation der pos sub ->
        and2 (derivation_ok trs der)
            (equalTerm (get   (stepResult (last der)) pos)
                       (apply (stepInput  (head der)) sub))

equalRule :: Rule -> Rule -> Bool
equalRule x y = case x of
  Rule xNames xLhs xRhs -> case y of 
    Rule yNames yLhs yRhs -> 
      and (Cons (equalList equalName xNames yNames)
          (Cons (equalTerm xLhs yLhs)
          (Cons (equalTerm xRhs yRhs)
          Nil)))

equalList :: (a -> a -> Bool) -> List a -> List a -> Bool
equalList eq xs ys = and2 (all (\n -> elem eq n ys) xs)
                          (all (\n -> elem eq n xs) ys)
    
elem :: (a -> a -> Bool) -> a -> List a -> Bool
elem eq x xs = case xs of
  Nil       -> False
  Cons y ys -> or2 (eq x y) (elem eq x ys)

equalTerm :: Term -> Term -> Bool
equalTerm x y = case x of
  V nX -> 
    case y of V nY     -> equalName nX nY
              Ap _ _   -> False
              F        -> False
              C        -> False
              N        -> False
              Foldr    -> False
  Ap aX bX ->
    case y of V _      -> False
              Ap aY bY -> and2 (equalTerm aX aY) (equalTerm bX bY)
              F        -> False
              C        -> False
              N        -> False
              Foldr    -> False
  F ->
    case y of V _      -> False
              Ap _ _   -> False
              F        -> True
              C        -> False
              N        -> False
              Foldr    -> False
  C ->
    case y of V _      -> False
              Ap _ _   -> False
              F        -> False
              C        -> True
              N        -> False
              Foldr    -> False
  N ->
    case y of V _      -> False
              Ap _ _   -> False
              F        -> False
              C        -> False
              N        -> True
              Foldr    -> False
  Foldr ->
    case y of V _      -> False
              Ap _ _   -> False
              F        -> False
              C        -> False
              N        -> False
              Foldr    -> True

equalName :: Name -> Name -> Bool
equalName x y = case x of 
  X -> 
    case y of X  -> True
              XS -> False
              G  -> False
              H  -> False
  XS -> 
    case y of X  -> False
              XS -> True
              G  -> False
              H  -> False
  G -> 
    case y of X  -> False
              XS -> False
              G  -> True
              H  -> False
  H -> 
    case y of X  -> False
              XS -> False
              G  -> False
              H  -> True

all :: (a -> Bool) -> List a -> Bool
all f xs = and (map f xs)

and :: List Bool -> Bool
and xs = foldr and2 True xs

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

null :: List a -> Bool
null xs = case xs of
    Nil -> True
    Cons x xs -> False

head :: List a -> a
head xs = case xs of
    Nil -> undefined
    Cons x xs -> x

last :: List a -> a
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
