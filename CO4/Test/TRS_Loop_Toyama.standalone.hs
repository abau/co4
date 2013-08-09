module TRS_Loop_Toyama where

import qualified Prelude

constraint :: Looping_Derivation -> Bool
constraint ld = looping_derivation_ok toyama ld

-- http://termcomp.uibk.ac.at/termcomp/tpdb/tpviewer.seam?tpId=54227&cid=3363
data Term = V Name | F Term Term Term | A | B | C

data Name = X | Y

data List a = Nil | Cons a (List a)

data Rule = Rule ( List Name ) -- variables
                 Term -- lhs
                 Term -- rhs

type TRS = List Rule

toyamaRule1 =
    Rule (Cons X Nil) (F A B (V X)) (F (V X) (V X) (V X))

toyamaRule2 = 
   Rule (Cons X (Cons Y Nil)) (F (V X) (V Y) C) (V X)

toyamaRule3 = 
   Rule (Cons X (Cons Y Nil)) (F (V X) (V Y) C) (V Y)

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

data Step = Step Term Rule Position Substitution Term

stepInput :: Step -> Term
stepInput step = case step of Step t _ _ _ _ -> t

stepResult :: Step -> Term
stepResult step = case step of Step _ _ _ _ t -> t

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind mA f = case mA of 
  Nothing -> Nothing
  Just a  -> f a

apply :: Term -> Substitution -> Maybe Term
apply term subs = case term of
  V v -> applyVar subs v
  F a b c -> bind (apply a subs) (\a' -> bind (apply b subs) (\b' -> bind (apply c subs) (\c' -> Just (F a' b' c'))))
  A -> Just A
  B -> Just B
  C -> Just C

applyVar :: Substitution -> Name -> Maybe Term
applyVar sub n = case sub of
  Nil -> Nothing
  Cons s ss -> case s of Pair name term -> case equalName name n of 
                                              False -> applyVar ss n
                                              True  -> Just term

replaceName :: Term -> Name -> Term -> Term
replaceName term name term' = case term of
  V n    -> case equalName n name of
              False -> term
              True  -> term'
  F a b c -> F (replaceName a name term') (replaceName b name term') (replaceName c name term')
  A      -> A
  B      -> B
  C      -> C

data Maybe a = Nothing | Just a

fmap :: (a -> b) -> Maybe a -> Maybe b
fmap f mA = case mA of Nothing -> Nothing
                       Just a  -> Just (f a)

get :: Term -> Position -> Maybe Term
get term pos = case pos of
  Nil -> Just term
  Cons p pos' -> case term of
    V _     -> Nothing
    F a b c -> case p of
      Pos1 -> get a pos'
      Pos2 -> get b pos'
      Pos3 -> get c pos'
    A       -> Nothing
    B       -> Nothing
    C       -> Nothing

put :: Term -> Position -> Term -> Maybe Term
put term pos term' = case pos of
  Nil -> Just term'
  Cons p pos' -> 
    case term of
      V _     -> Nothing
      F a b c -> case p of 
        Pos1 -> fmap (\a' -> F a' b c) (put a pos' term')
        Pos2 -> fmap (\b' -> F a b' c) (put b pos' term')
        Pos3 -> fmap (\c' -> F a b c') (put c pos' term')
      A       -> Nothing
      B       -> Nothing
      C       -> Nothing

rule_ok :: TRS -> Rule -> Bool
rule_ok trs rule = elem equalRule rule trs

fromMaybe a mA = case mA of Nothing -> a
                            Just a' -> a'
  
step_ok :: TRS -> Step -> Bool
step_ok trs s = case s of
  Step t0 rule pos sub t1 -> 
    and2 (rule_ok trs rule)
     (case rule of
        Rule vars lhs rhs -> 
          fromMaybe False (
            bind (get t0 pos) (\g -> bind (apply lhs sub) (\lhs' -> bind (apply rhs sub) (\rhs' -> bind (put t0 pos rhs') (\p -> 
                Just (and2 (equalTerm g lhs') (equalTerm p t1))
                ))))
          )
      )

type Derivation = List Step

derivation_ok trs deriv = case deriv of
  Nil -> False
  Cons s _ -> case s of
    Step t _ _ _ _ -> case derive_ok trs t deriv of
                        Nothing -> False
                        Just _  -> True

derive_ok :: TRS -> Term -> Derivation -> Maybe Term
derive_ok trs term deriv = case deriv of
  Nil           -> Just term
  Cons s deriv' -> case s of
    Step t0 rule pos sub t1 -> case equalTerm term t0 of
      False -> Nothing
      True  -> case step_ok trs s of False -> Nothing
                                     True  -> derive_ok trs t1 deriv'


data Looping_Derivation = Looping_Derivation Derivation Position Substitution

looping_derivation_ok :: TRS -> Looping_Derivation -> Bool
looping_derivation_ok trs ld = case ld of
    Looping_Derivation der pos sub ->
      case der of
        Nil -> False
        Cons s _ -> case s of
          Step t0 _ _ _ _ ->
            case derive_ok trs t0 der of
              Nothing -> False
              Just last -> 

                fromMaybe False (
                    bind (get last pos) (\term ->
                      bind (apply t0 sub) (\term' ->
                        Just (equalTerm term' term)
                        )
                      )
                  )
                  
equalRule :: Rule -> Rule -> Bool
equalRule x y = case x of
  Rule xNames xLhs xRhs -> case y of 
    Rule yNames yLhs yRhs -> 
      and2 (equalTerm xLhs yLhs) (equalTerm xRhs yRhs)

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
              A        -> False
              B        -> False
              C        -> False
  F a b c ->
    case y of V _      -> False
              F x y z  -> and2 (equalTerm a x) (and2 (equalTerm b y) (equalTerm c z))
              A        -> False
              B        -> False
              C        -> False
  A ->
    case y of V _      -> False
              F _ _ _  -> False
              A        -> True
              B        -> False
              C        -> False
  B ->
    case y of V _      -> False
              F _ _ _  -> False
              A        -> False
              B        -> True
              C        -> False
  C ->
    case y of V _      -> False
              F _ _ _  -> False
              A        -> False
              B        -> False
              C        -> True

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
