module CO4.Test.TermComp2014.Data
where

import Prelude hiding (Ordering)

-- Common
type Map k v = [(k,v)]

type Symbol = [Bool]

data Term = Var  Symbol
          | Node Symbol [Term]

data Rule = Rule Term Term

data Trs  = Trs [Rule]

-- SL

type Domain  = [Bool]

type Interpretation = Map [Domain] Domain

type Model       = Map Symbol Interpretation

type Sigma       = Map Symbol Domain

type Assignments = [Sigma]

-- LPO
data Order = Gr | Eq | NGe

data Nat   = Zero | Succ Nat

type Precedence = Map Symbol Nat
