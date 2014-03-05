module CO4.Test.TermComp2014.Standalone
where

import Prelude hiding (lex,lookup,length)

type Map k v        = [(k,v)]

type Symbol         = [Bool]

type Domain         = [Bool]

data Term label     = Var  Symbol
                    | Node Symbol label [Term label]
                    deriving (Eq,Show)

data Rule label     = Rule (Term label) (Term label)
                    deriving (Eq,Show)

data Trs label      = Trs [Rule label]
                    deriving (Eq,Show)

type Interpretation = Map [Domain] Domain

type Model          = Map Symbol Interpretation

type Sigma          = Map Symbol Domain

type Assignments    = [Sigma]

data Order          = Gr | Eq | NGe
                    deriving (Eq,Show)

data Nat            = Zero | Succ Nat
                    deriving (Eq,Show)

type PrecedenceKey  = (Symbol, [Domain])

type Precedence     = Map PrecedenceKey Nat

constraint :: (Trs (),Assignments) -> (Model,Trs [Domain],Precedence) -> Bool
constraint (trs,assignments) (model, labeledTrs, precedence) = 
  and [ isModelForTrsUnderAllAssignments model trs assignments 
      , eqTrs eqLabel labeledTrs (makeLabeledTrs model trs assignments)
      , isMonotoneAccordingLPO labeledTrs precedence
      ]

-- * search model

isModelForTrsUnderAllAssignments :: Model -> Trs () -> Assignments -> Bool
isModelForTrsUnderAllAssignments model trs assignments =
  all (isModelForTrs model trs) assignments

isModelForTrs :: Model -> Trs () -> Sigma -> Bool
isModelForTrs model (Trs rules) sigma = all (isModelForRule model sigma) rules

isModelForRule :: Model -> Sigma -> Rule () -> Bool
isModelForRule model sigma (Rule lhs rhs) = 
  eqValue (valueOfTerm model sigma lhs)
          (valueOfTerm model sigma rhs)

valueOfTerm :: Model -> Sigma -> Term () -> Domain
valueOfTerm model sigma term = case term of
  Var v           -> valueOfVar v sigma
  Node sym l args -> case l of 
    () -> let values = map (valueOfTerm model sigma) args
          in
            valueOfFun sym values model

valueOfFun :: Symbol -> [Domain] -> Model -> Domain
valueOfFun s args model = 
  let interp = interpretation s model
  in
    lookup (\xs ys -> and (zipWith eqValue xs ys)) args interp

valueOfVar :: Symbol -> Sigma -> Domain
valueOfVar = lookup eqSymbol

interpretation :: Symbol -> Model -> Interpretation
interpretation = lookup eqSymbol

-- * make labeled TRS

makeLabeledTrs :: Model -> Trs () -> Assignments -> Trs [Domain]
makeLabeledTrs model (Trs rules) assignments = 
  let goRules sigma                 = map (goRule sigma) rules
      goRule  sigma (Rule lhs rhs)  = Rule (fst (goTerm sigma lhs)) (fst (goTerm sigma rhs))
      goTerm  sigma term            = case term of
        Var s         -> (Var s, valueOfVar s sigma)
        Node s l args -> case l of 
          () -> case unzip (map (goTerm sigma) args) of
            (args', argsValues) -> (Node s argsValues args', valueOfFun s argsValues model)
  in
    Trs (concatMap goRules assignments)

-- * search precedence

isMonotoneAccordingLPO :: Trs [Domain] -> Precedence -> Bool
isMonotoneAccordingLPO (Trs rules) precedence = 
  all (\(Rule lhs rhs) -> eqOrder (lpo (ord precedence) lhs rhs) Gr) rules

lpo :: (PrecedenceKey -> PrecedenceKey -> Order) -> Term [Domain] -> Term [Domain] -> Order
lpo ord s t = case t of
  Var x -> case eqTerm eqLabel s t of 
    False -> case varOccurs x s of
                False -> NGe
                True  -> Gr
    True  -> Eq

  Node g lg ts  -> case s of
    Var _     -> NGe
    Node f lf ss -> 
      case all (\si -> eqOrder (lpo ord si t) NGe) ss of
        False -> Gr
        True  -> case ord (f,lf) (g,lg) of
                    Gr  -> case all (\ti -> eqOrder (lpo ord s ti) Gr) ts of
                             False -> NGe
                             True  -> Gr
                    Eq  -> case all (\ti -> eqOrder (lpo ord s ti) Gr) ts of
                             False -> NGe
                             True  -> lex (lpo ord) ss ts
                    NGe -> NGe

ord :: Precedence -> PrecedenceKey -> PrecedenceKey -> Order
ord prec a b = 
  let pa = lookup eqPrecedenceKey a prec
      pb = lookup eqPrecedenceKey b prec
  in
    ordNat pa pb

ordNat :: Nat -> Nat -> Order
ordNat a b = case a of
  Zero    -> case b of Zero    -> Eq
                       _       -> NGe
  Succ a' -> case b of Zero    -> Gr
                       Succ b' -> ordNat a' b'

varOccurs :: Symbol -> Term a -> Bool
varOccurs var term = case term of
  Var var'    -> eqSymbol var var'
  Node _ _ ts -> any (varOccurs var) ts

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

length :: [a] -> Nat
length = foldr (\_ -> Succ) Zero

eqTrs :: (a -> a -> Bool) -> Trs a -> Trs a -> Bool
eqTrs f (Trs ra) (Trs rb) = and [ and (zipWith (eqRule f) ra rb)
                                , eqNat (length ra) (length rb)
                                ]

eqRule :: (a -> a -> Bool) -> Rule a -> Rule a -> Bool
eqRule f (Rule la ra) (Rule lb rb) = (eqTerm f la lb) && (eqTerm f ra rb)

eqTerm :: (a -> a -> Bool) -> Term a -> Term a -> Bool
eqTerm f x y = case x of
  Var u     -> case y of { Var v -> eqSymbol u v; _ -> False }
  Node u lu us -> case y of
    Node v lv vs -> and [ eqSymbol u v
                        , f lu lv
                        , eqList (eqTerm f) us vs ]
    _            -> False

eqLabel :: [Domain] -> [Domain] -> Bool
eqLabel = eqList eqValue

eqOrder :: Order -> Order -> Bool
eqOrder x y = case x of
  Gr  -> case y of { Gr  -> True; _ -> False }
  Eq  -> case y of { Eq  -> True; _ -> False }
  NGe -> case y of { NGe -> True; _ -> False }

eqPrecedenceKey :: PrecedenceKey -> PrecedenceKey -> Bool
eqPrecedenceKey (s,l) (s',l') = (eqSymbol s s') && (eqLabel l l')

eqNat :: Nat -> Nat -> Bool
eqNat a b = case a of
  Zero    -> case b of { Zero    -> True       ; _ -> False }
  Succ a' -> case b of { Succ b' -> eqNat a' b'; _ -> False }

eqSymbol :: Symbol -> Symbol -> Bool
eqSymbol = eqList eqBool

eqValue :: Domain -> Domain -> Bool
eqValue = eqList eqBool

eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList f xs ys = case xs of
  []   -> case ys of []   -> True
                     _    -> False
  u:us -> case ys of []   -> False
                     v:vs -> (f u v) && (eqList f us vs)

eqBool :: Bool -> Bool -> Bool
eqBool x y = case x of
  False -> not y
  True  -> y
