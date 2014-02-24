module CO4.Test.TermComp2014.Standalone
where

type Symbol  = [Bool]

type Domain  = [Bool]

type Mapping = ([Domain], Domain)

type Interpretation = [Mapping]

type Model       = [(Symbol, Interpretation)]

type Sigma       = [(Symbol, Domain)]

type Assignments = [Sigma]

data Term = Var  Symbol
          | Node Symbol [Term]

data Rule = Rule Term Term

data Trs  = Trs [Rule]

constraint :: (Trs,Assignments) -> Model -> Bool
constraint (trs,assignments) model = all (isModelForTrs model trs) assignments

isModelForTrs :: Model -> Trs -> Sigma -> Bool
isModelForTrs model (Trs rules) sigma = all (isModelForRule model sigma) rules

isModelForRule :: Model -> Sigma -> Rule -> Bool
isModelForRule model sigma (Rule lhs rhs) = 
  eqValue (valueOfTerm model sigma lhs)
          (valueOfTerm model sigma rhs)

valueOfTerm :: Model -> Sigma -> Term -> Domain
valueOfTerm model sigma term = case term of
  Var v         -> valueOfVar v sigma
  Node sym args -> 
    let values = map (valueOfTerm model sigma) args
        i      = interpretation model sym
    in
      mapping i values

valueOfVar :: Symbol -> Sigma -> Domain
valueOfVar symbol sigma = case sigma of
  []   -> undefined
  s:ss -> case s of 
    (symbol', value) ->
      case eqSymbol symbol symbol' of
        False -> valueOfVar symbol ss
        True  -> value

mapping :: Interpretation -> [Domain] -> Domain
mapping interpretation values = case interpretation of
  []   -> undefined
  i:is -> case i of
    (xs,y) -> case and (zipWith eqValue xs values) of
                False -> mapping is values
                True  -> y

interpretation :: Model -> Symbol -> Interpretation
interpretation model symbol = case model of
  []   -> undefined
  m:ms -> case m of
    (sym, i) -> case eqSymbol sym symbol of
      False -> interpretation ms symbol
      True  -> i

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
