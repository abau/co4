
module TRS_Loop where

import qualified Prelude
import           Prelude (undefined)

main :: Looping_Derivation -> Bool
main ld = looping_derivation_ok toyama ld

-- http://termcomp.uibk.ac.at/termcomp/tpdb/tpviewer.seam?tpId=54227&cid=3363
data Term = V Name | F Term Term Term | G Term Term | A | B

data Name = X | Y

data List a = Nil | Cons a (List a)

data Rule = Rule ( List Name ) -- variables
                 Term -- lhs
                 Term -- rhs

type TRS = List Rule

toyamaRule1 =
    Rule (Cons X Nil) (F A B (V X)) (F (V X) (V X) (V X))

toyamaRule2 = 
   Rule (Cons X (Cons Y Nil)) (G (V X) (V Y)) (V X)

toyamaRule3 = 
   Rule (Cons X (Cons Y Nil)) (G (V X) (V Y)) (V Y)

toyama :: TRS
toyama = Cons toyamaRule1 (Cons toyamaRule2 (Cons toyamaRule3 Nil))
		
data Bool = False | True
  --deriving Prelude.Show
data Pos = Pos1 | Pos2 | Pos3
type Position = List Pos 

data Pair a b = Pair a b

fst :: Pair a b -> a
fst p = case p of Pair a _ -> a

snd :: Pair a b -> b
snd p = case p of Pair _ b -> b

type Substitution = List (Pair Name Term)

domain :: Substitution -> List Name
domain = map fst

--data Step = Step Term Rule Position Substitution Term
data Step = Step Term Rule (List Pos) (List (Pair Name Term)) Term

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
  F a b c -> F (replaceName a name term') (replaceName b name term') (replaceName c name term')
  G a b   -> G (replaceName a name term') (replaceName b name term') 
  A      -> A
  B      -> B

data Maybe a = Nothing | Just a

get :: Term -> Position -> Term
get term pos = case pos of
  Nil -> term
  Cons p pos' -> case term of
    V _     -> undefined
    F a b c -> case p of
      Pos1 -> get a pos'
      Pos2 -> get b pos'
      Pos3 -> get c pos'
    G a b -> case p of
      Pos1 -> get a pos'
      Pos2 -> get b pos'
      Pos3 -> undefined
    A       -> undefined
    B       -> undefined

put :: Term -> Position -> Term -> Term
put term pos term' = case pos of
  Nil -> term'
  Cons p pos' -> 
    case term of
      V _     -> undefined
      F a b c -> case p of 
        Pos1 -> F (put a pos' term') b c
        Pos2 -> F a (put b pos' term') c
        Pos3 -> F a b (put c pos' term')
      G a b -> case p of 
        Pos1 -> G (put a pos' term') b 
        Pos2 -> G a (put b pos' term')
        Pos3 -> undefined
      A       -> undefined
      B       -> undefined

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
                                             (List Pos) 
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
              F _ _ _  -> False
              G _ _    -> False
              A        -> False
              B        -> False
  F a b c ->
    case y of V _      -> False
              F x y z  -> and2 (equalTerm a x) (and2 (equalTerm b y) (equalTerm c z))
              G _ _    -> False
              A        -> False
              B        -> False
  G a b ->
    case y of V _      -> False
              F _ _ _  -> False
              G x y    -> and2 (equalTerm a x) (equalTerm b y)
              A        -> False
              B        -> False
  A ->
    case y of V _      -> False
              F _ _ _  -> False
              G _ _    -> False
              A        -> True
              B        -> False
  B ->
    case y of V _      -> False
              F _ _ _  -> False
              G _ _    -> False
              A        -> False
              B        -> True

equalName :: Name -> Name -> Bool
equalName x y = case x of 
  X -> 
    case y of X -> True
              Y -> False
  Y -> 
    case y of X -> False
              Y -> True

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
