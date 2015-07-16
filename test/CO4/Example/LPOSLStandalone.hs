module CO4.Example.LPOSLStandalone
where

import Prelude hiding (lex,lookup)
import CO4.Prelude.Nat

type Map k v        = [(k,v)]

type Symbol         = Nat

data Term           = Var Symbol
                    | Node Symbol [Term]

type Label          = [Nat]
data LTerm          = LVar Symbol
                    | LNode Symbol Label [LTerm]

data Order          = Gr | Eq | NGe

type Trs            = [(Term,Term)]
type LTrs           = [(LTerm,LTerm)]
type Precedence     = Map (Symbol,Label) Nat
type Function       = Map [Nat] Nat
type Interpretation = Map Symbol Function
type Sigma          = Map Symbol Nat

constraint :: (Trs, [Sigma]) -> (Precedence, Interpretation) -> Bool
constraint (trs, assignments) (precedence, interpretation) = 
  let ltrs = labelledTrs interpretation assignments trs
  in
    and [ all (\(lhs,rhs) -> eqOrder (lpo precedence lhs rhs) Gr) ltrs
        , isModel interpretation assignments trs ]

-- lpo

lpo :: Precedence -> LTerm -> LTerm -> Order
lpo precedence s t = case t of
  LVar x -> case eqLTerm s t of 
    False -> case varOccurs x s of
                False -> NGe
                True  -> Gr
    True  -> Eq

  LNode g gl ts -> case s of
    LVar _ -> NGe
    LNode f fl ss -> 
      case all (\si -> eqOrder (lpo precedence si t) NGe) ss of
        False -> Gr
        True  -> case ord precedence (f,fl) (g,gl) of
                    Gr  -> case all (\ti -> eqOrder (lpo precedence s ti) Gr) ts of
                             False -> NGe
                             True  -> Gr
                    Eq  -> case all (\ti -> eqOrder (lpo precedence s ti) Gr) ts of
                             False -> NGe
                             True  -> lex (lpo precedence) ss ts
                    NGe -> NGe

ord :: Precedence -> (Symbol,Label) -> (Symbol,Label) -> Order
ord precedence a b = 
  let pa = lookup eqLSymbol a precedence
      pb = lookup eqLSymbol b precedence
  in
    ordNat pa pb

ordNat :: Nat -> Nat -> Order
ordNat a b = case eqNat a b of
  True  -> Eq
  False -> case gtNat a b of
    True  -> Gr
    False -> NGe

varOccurs :: Symbol -> LTerm -> Bool
varOccurs var term = case term of
  LVar var'    -> eqSymbol var var'
  LNode _ _ ts -> any (varOccurs var) ts

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

-- utilities

lookup :: (k -> k -> Bool) -> k -> Map k v -> v
lookup f k map = case map of
  []   -> undefined
  m:ms -> case m of 
    (k',v) -> case f k k' of
      False -> lookup f k ms
      True  -> v

eqLabel :: Label -> Label -> Bool
eqLabel x y = eqList eqNat x y

eqLTerm :: LTerm -> LTerm -> Bool
eqLTerm x y = case x of
  LVar u -> case y of { LVar v -> eqSymbol u v; _ -> False }
  LNode u ul us -> case y of
    LNode v vl vs -> (eqLSymbol (u,ul) (v,vl)) && (eqList eqLTerm us vs)
    _ -> False

eqOrder :: Order -> Order -> Bool
eqOrder x y = case x of
  Gr  -> case y of { Gr  -> True; _ -> False }
  Eq  -> case y of { Eq  -> True; _ -> False }
  NGe -> case y of { NGe -> True; _ -> False }

eqLSymbol :: (Symbol,Label) -> (Symbol,Label) -> Bool
eqLSymbol (x,xl) (y,yl) = (eqSymbol x y) && (eqLabel xl yl)

eqSymbol :: Symbol -> Symbol -> Bool
eqSymbol = eqNat

eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList f xs ys = case xs of
  []   -> case ys of []   -> True
                     _    -> False
  u:us -> case ys of []   -> False
                     v:vs -> (f u v) && (eqList f us vs)

-- semantic labelling

labelledTrs :: Interpretation -> [Sigma] -> Trs -> LTrs
labelledTrs interpretation assignments = 
  concatMap (\(lhs,rhs) -> 
    map (\sigma -> ( labelledTerm interpretation sigma lhs
                   , labelledTerm interpretation sigma rhs )
        ) assignments
  )

labelledTerm :: Interpretation -> Sigma -> Term -> LTerm
labelledTerm interpretation sigma t = case t of
  Var v     -> LVar v
  Node f ts -> let as  = map (eval interpretation sigma) ts
                   ts' = map (labelledTerm interpretation sigma) ts
               in
                 LNode f as ts'

isModel :: Interpretation -> [Sigma] -> Trs -> Bool
isModel interpretation assignments trs =
  all (\sigma -> 
    all (\(lhs,rhs) -> eqNat (eval interpretation sigma lhs)
                             (eval interpretation sigma rhs)
        ) trs
  ) assignments

eval :: Interpretation -> Sigma -> Term -> Nat
eval interpretation sigma t = case t of
  Var  v    -> lookup eqSymbol v sigma
  Node f ts -> let i  = lookup eqSymbol f interpretation
                   as = map (eval interpretation sigma) ts
               in
                 lookup (eqList eqNat) as i
