module CO4.Test.TermComp2014.Standalone
where

import Prelude hiding (lex,lookup,length)
import CO4.PreludeNat
import CO4.PreludeBool (xor2)

type Map k v               = [(k,v)]

type Symbol                = [Bool]

type Domain                = [Bool]

type Label                 = [Domain]

data Term var node label   = Var  var
                           | Node node label [Term var node label]
                           deriving (Eq,Show)

data Rule var node label   = Rule (Term var node label) (Term var node label)
                           deriving (Eq,Show)

data Trs var node label    = Trs [Rule var node label]
                           deriving (Eq,Show)

type UnlabeledTerm         = Term Symbol Symbol ()
type UnlabeledRule         = Rule Symbol Symbol ()
type UnlabeledTrs          = Trs  Symbol Symbol ()

type MarkedSymbol          = (Symbol, Bool)

type DPTerm label          = Term Symbol MarkedSymbol label
type DPRule label          = Rule Symbol MarkedSymbol label
data DPTrs label           = DPTrs [ [DPRule label] ]
                           deriving (Eq,Show)

type LabeledTerm           = Term Symbol Symbol Label
type LabeledRule           = Rule Symbol Symbol Label
type LabeledTrs            = Trs  Symbol Symbol Label

type Interpretation        = Map [Domain] Domain

type Model sym             = Map sym Interpretation

type Sigma sym             = Map sym Domain

type Assignments sym       = [Sigma sym]

data Order                 = Gr | Eq | NGe
                           deriving (Eq,Show)

type Precedence sym label  = Map (sym, label) Nat

constraint :: (DPTrs (), Assignments Symbol) 
           -> (Model MarkedSymbol, Precedence MarkedSymbol Label) -> Bool
constraint (trs,assignments) (model, precedence) = 
  let labeledTrs = makeLabeledTrs model trs assignments
  in
    and [ isModelForTrsUnderAllAssignments model trs assignments 
        , allRulesDecreasing         labeledTrs precedence
        , existsStrongDecreasingRule labeledTrs precedence
        ]

-- * search model

isModelForTrsUnderAllAssignments :: Model MarkedSymbol -> DPTrs () -> Assignments Symbol -> Bool
isModelForTrsUnderAllAssignments model trs assignments =
  all (isModelForTrs model trs) assignments

isModelForTrs :: Model MarkedSymbol -> DPTrs () -> Sigma Symbol -> Bool
isModelForTrs model (DPTrs rules) sigma = all (all (isModelForRule model sigma)) rules

isModelForRule :: Model MarkedSymbol -> Sigma Symbol -> DPRule () -> Bool
isModelForRule model sigma (Rule lhs rhs) = 
  eqValue (valueOfTerm model sigma lhs)
          (valueOfTerm model sigma rhs)

valueOfTerm :: Model MarkedSymbol -> Sigma Symbol -> DPTerm () -> Domain
valueOfTerm model sigma term = case term of
  Var v           -> valueOfVar v sigma
  Node sym l args -> case l of 
    () -> let values = map (valueOfTerm model sigma) args
          in
            valueOfFun sym values model

valueOfFun :: MarkedSymbol -> [Domain] -> Model MarkedSymbol -> Domain
valueOfFun s args model = 
  let interp = interpretation s model
  in
    lookup (\xs ys -> and (zipWith eqValue xs ys)) args interp

valueOfVar :: Symbol -> Sigma Symbol -> Domain
valueOfVar = lookup eqSymbol

interpretation :: MarkedSymbol -> Model MarkedSymbol -> Interpretation
interpretation = lookup eqMarkedSymbol

-- * make labeled TRS

makeLabeledTrs :: Model MarkedSymbol -> DPTrs () -> Assignments Symbol -> DPTrs Label
makeLabeledTrs model (DPTrs rules) assignments = 
  let goRules                       = concatMap (\r -> map (goRule r) assignments)
      goRule  (Rule lhs rhs) sigma  = Rule (fst (goTerm lhs sigma)) (fst (goTerm rhs sigma))
      goTerm  term           sigma  = case term of
        Var s         -> (Var s, valueOfVar s sigma)
        Node s l args -> case l of 
          () -> case unzip (map (\t -> goTerm t sigma) args) of
            (args', argsValues) -> (Node s argsValues args', valueOfFun s argsValues model)
  in
    DPTrs (map goRules rules)

-- * search precedence

allRulesDecreasing :: DPTrs Label -> Precedence MarkedSymbol Label -> Bool
allRulesDecreasing (DPTrs rules) precedence =
  all (all (isDecreasingRule precedence)) rules

isDecreasingRule :: Precedence MarkedSymbol Label -> DPRule Label -> Bool
isDecreasingRule precedence (Rule lhs rhs) = 
  case lpo (ord precedence) lhs rhs of
    Gr  -> True
    Eq  -> True
    NGe -> False

existsStrongDecreasingRule :: DPTrs Label -> Precedence MarkedSymbol Label -> Bool
existsStrongDecreasingRule (DPTrs rules) precedence =
  exists rules (all (isMarkedStrongDecreasingRule precedence))

isMarkedStrongDecreasingRule :: Precedence MarkedSymbol Label -> DPRule Label -> Bool
isMarkedStrongDecreasingRule precedence (Rule lhs rhs) = 
  (isMarked lhs) && (eqOrder (lpo (ord precedence) lhs rhs) Gr)

isMarked :: DPTerm label -> Bool
isMarked term = case term of 
  Var _          -> False
  Node (_,m) _ _ -> m

lpo :: ((MarkedSymbol, Label) -> (MarkedSymbol, Label) -> Order) 
    -> DPTerm Label -> DPTerm Label -> Order
lpo ord s t = case t of
  Var x -> case eqLabeledDPTerm s t of 
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

ord :: Precedence MarkedSymbol Label -> (MarkedSymbol, Label) -> (MarkedSymbol, Label) -> Order
ord prec a b = 
  let key (s,l) (s',l') = (eqMarkedSymbol s s') && (eqLabel l l')

      pa = lookup key a prec
      pb = lookup key b prec
  in
    ordNat pa pb

ordNat :: Nat -> Nat -> Order
ordNat a b = case eqNat a b of
  True  -> Eq
  False -> case gtNat a b of
    True  -> Gr
    False -> NGe

varOccurs :: Symbol -> Term Symbol node label -> Bool
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

eqLabeledDPTerm :: DPTerm Label -> DPTerm Label -> Bool
eqLabeledDPTerm = eqTerm eqSymbol eqMarkedSymbol eqLabel

eqTerm :: (v -> v -> Bool) -> (n -> n -> Bool) -> (l -> l -> Bool) 
       -> Term v n l -> Term v n l -> Bool
eqTerm f_var f_node f_label x y = case x of
  Var u     -> case y of { Var v -> f_var u v; _ -> False }
  Node u lu us -> case y of
    Node v lv vs -> and [ f_node u v
                        , f_label lu lv
                        , eqList (eqTerm f_var f_node f_label) us vs ]
    _            -> False

eqLabel :: Label -> Label -> Bool
eqLabel = eqList eqValue

eqOrder :: Order -> Order -> Bool
eqOrder x y = case x of
  Gr  -> case y of { Gr  -> True; _ -> False }
  Eq  -> case y of { Eq  -> True; _ -> False }
  NGe -> case y of { NGe -> True; _ -> False }

eqMarkedSymbol :: MarkedSymbol -> MarkedSymbol -> Bool
eqMarkedSymbol (sym,m) (sym',m') = (eqSymbol sym sym') && (eqBool m m')

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
eqBool x y = not (xor2 x y)

exists :: [a] -> (a -> Bool) -> Bool
exists xs f = any f xs
