module CO4.Test.TermComp2014.Data
where

import Prelude hiding (Ordering)

-- Common
type Map k v = [(k,v)]

type Symbol = [Bool]

data Term = Var  Symbol
          | Node Symbol [Term]
          deriving (Eq,Show)

data Rule = Rule Term Term
          deriving (Eq,Show)

data Trs  = Trs [Rule]
          deriving (Eq,Show)

-- SL

type Domain  = [Bool]

type Interpretation = Map [Domain] Domain

type Model       = Map Symbol Interpretation

type Sigma       = Map Symbol Domain

type Assignments = [Sigma]

-- LPO
data Order = Gr | Eq | NGe
           deriving (Eq,Show)

data Nat   = Zero | Succ Nat
           deriving (Eq,Show)

type Precedence = Map Symbol Nat
