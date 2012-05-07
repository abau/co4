{-# LANGUAGE DeriveDataTypeable #-}
module CO4.Language
  ( Name (..), Literal (..), Pattern (..), Match (..), Expression (..), Type (..)
  , Scheme (..), Declaration (..), Program
  ) 
where

import           Data.Data (Data,Typeable)

data Type = TVar Name
          | TCon Name [Type]
          deriving (Show,Eq,Ord,Data,Typeable)

data Scheme = SType Type
            | SForall Name Scheme
            deriving (Show,Eq,Ord,Data,Typeable)

data Name = Name      String
          | TypedName String Scheme
          deriving (Show,Data,Typeable)

{-
instance Eq Name where
  (Name a) == (Name b)                 = a == b
  (TypedName a ta) == (TypedName b tb) = (a == b) && (ta == tb)
  (Name name) == _                     = error $ "Do not compare untyped names against typed names (here: '" ++ name ++ "')"
  a == b                               = b == a
-}

instance Eq Name where
  a == b = let string (Name n)        = n
               string (TypedName n _) = n
           in
             string a == string b

instance Ord Name where
  a `compare` b = let string (Name n)        = n
                      string (TypedName n _) = n
                  in
                    compare (string a) (string b)

data Literal = LInt    Integer
             | LChar   Char
             | LDouble Double
             deriving (Show,Eq,Ord,Data,Typeable)

data Pattern = PVar Name
             | PLit Literal
             | PCon Name [Pattern]
             deriving (Show,Eq,Ord,Data,Typeable)

data Match = Match Pattern Expression
             deriving (Show,Eq,Ord,Data,Typeable)

data Expression = EVar  Name
                | ECon  Name
                | ELit  Literal
                | EApp  Expression [Expression]
                | ETApp Expression [Type]
                | ELam  [Name] Expression
                | ETLam [Name] Expression
                | ECase Expression [Match]
                | ELet  Name Expression Expression
                deriving (Show,Eq,Ord,Data,Typeable)

data Declaration = DBind Name Expression 
                deriving (Show,Eq,Data,Typeable)

type Program     = [Declaration]


