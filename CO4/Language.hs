{-# LANGUAGE DeriveDataTypeable #-}
module CO4.Language
  ( Type (..), Scheme (..)
  , Name (..), UntypedName (..)
  , Literal (..), Pattern (..), Match (..), Expression (..)
  , Constructor (..), Declaration (..), Program
  ) 
where

import           Data.Data (Data,Typeable)

data Type = TVar UntypedName
          | TCon UntypedName [Type]
          deriving (Show,Eq,Ord,Data,Typeable)

data Scheme = SType Type
            | SForall UntypedName Scheme
            deriving (Show,Eq,Ord,Data,Typeable)

data UntypedName = UntypedName String
                 deriving (Show,Eq,Ord,Data,Typeable)

data Name = NUntyped String
          | NTyped String Scheme
          deriving (Show,Data,Typeable)

instance Eq Name where
  a == b = let string (NUntyped n) = n
               string (NTyped n _) = n
           in
             string a == string b

instance Ord Name where
  a `compare` b = let string (NUntyped n) = n
                      string (NTyped n _) = n
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
                | ETLam [UntypedName] Expression
                | ECase Expression [Match]
                | ELet  Name Expression Expression
                deriving (Show,Eq,Ord,Data,Typeable)

data Constructor = CCon UntypedName [Type]
                deriving (Show,Eq,Ord,Data,Typeable)

data Declaration = DBind { dBindName :: Name , dBindExpression :: Expression }
                 | DAdt  { dAdtName          ::  UntypedName 
                         , dAdtTypeVariables :: [UntypedName] 
                         , dAdtConstructors  :: [Constructor] }
                deriving (Show,Eq,Data,Typeable)

type Program     = [Declaration]

