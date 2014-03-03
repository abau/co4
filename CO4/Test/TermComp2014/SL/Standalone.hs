module CO4.Test.TermComp2014.SL.Standalone
where

import Prelude hiding (lookup)

-- Keep in sync with CO4.Test.TermComp2014.Data
type Map k v = [(k,v)]

type Symbol  = [Bool]

data Term = Var  Symbol
          | Node Symbol [Term]

data Rule = Rule Term Term

data Trs  = Trs [Rule]

type Domain  = [Bool]

type Interpretation = Map [Domain] Domain

type Model       = Map Symbol Interpretation

type Sigma       = Map Symbol Domain

type Assignments = [Sigma]

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
        i      = interpretation sym model
    in
      mapping values i

valueOfVar :: Symbol -> Sigma -> Domain
valueOfVar = lookup eqSymbol

mapping :: [Domain] -> Interpretation -> Domain
mapping = lookup (\xs ys -> and (zipWith eqValue xs ys))

interpretation :: Symbol -> Model -> Interpretation
interpretation = lookup eqSymbol

lookup :: (k -> k -> Bool) -> k -> Map k v -> v
lookup f k map = case map of
  []   -> undefined
  m:ms -> case m of 
    (k',v) -> case f k k' of
      False -> lookup f k ms
      True  -> v

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
