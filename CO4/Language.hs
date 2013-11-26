{-# LANGUAGE DeriveDataTypeable #-}
module CO4.Language
  ( Type (..), Scheme (..)
  , Name (..), UntypedName (..)
  , Pattern (..), Match (..), Binding (..), Expression (..)
  , Constructor (..), Adt (..), Declaration (..), Program (..)
  ) 
where

import Data.Data (Data,Typeable)

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

data Type = TVar UntypedName
          | TCon UntypedName [Type]
          deriving (Show,Eq,Ord,Data,Typeable)

data Scheme = SType Type
            | SForall UntypedName Scheme
            deriving (Show,Eq,Ord,Data,Typeable)

data Pattern = PVar Name
             | PCon Name [Pattern]
             deriving (Show,Eq,Ord,Data,Typeable)

data Match = Match { matchPattern    :: Pattern 
                   , matchExpression :: Expression
                   }
             deriving (Show,Eq,Ord,Data,Typeable)

data Binding = Binding { boundName       :: Name 
                       , boundExpression :: Expression }
             deriving (Show,Eq,Ord,Data,Typeable)

data Expression = EVar  Name
                | ECon  Name
                | EApp  Expression [Expression]
                | ETApp Expression [Type]
                | ELam  [Name] Expression
                | ETLam [UntypedName] Expression
                | ECase Expression [Match]
                | ELet  [Binding] Expression
                | EUndefined
                deriving (Show,Eq,Ord,Data,Typeable)

data Constructor = CCon { cConName          :: UntypedName 
                        , cConArgumentTypes :: [Type]
                        }
                deriving (Show,Eq,Ord,Data,Typeable)

data Adt = Adt { adtName          ::  UntypedName 
               , adtTypeVariables :: [UntypedName] 
               , adtConstructors  :: [Constructor] 
               }
               deriving (Show,Eq,Ord,Data,Typeable)

data Declaration = DBind Binding
                 | DAdt  Adt
                deriving (Show,Eq,Data,Typeable)

data Program = Program { pMain  :: Binding
                       , pDecls :: [Declaration] } 
             deriving (Show,Eq,Data,Typeable)

