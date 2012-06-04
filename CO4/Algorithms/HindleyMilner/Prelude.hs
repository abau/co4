module CO4.Algorithms.HindleyMilner.Prelude
  (prelude, freeInPrelude)
where

import           Data.List ((\\))
import           CO4.Algorithms.HindleyMilner.Util (Context(..),gamma,toList)
import           CO4.Algorithms.Free (free)
import           CO4.Names
import           CO4.Language
import           CO4.Frontend.String ()
import           CO4.Frontend (SchemeFrontend (parseScheme))

prelude :: Context
prelude = gamma
  [ (consCon,       parseScheme "forall a . a -> List a -> List a")
  , (nilCon,        parseScheme "forall a . List a")
  , (trueCon,       SType $ TCon boolType [])
  , (falseCon,      SType $ TCon boolType [])
  , (tupleCon 2,    parseScheme "forall a . forall b . a -> b -> Tuple2 a b")
  , (tupleCon 3,    parseScheme "forall a . forall b . forall c . a -> b -> c -> Tuple3 a b c")
  , (tupleCon 4,    parseScheme "forall a . forall b . forall c . forall d . a -> b -> c -> d -> Tuple4 a b c d")
  , (tupleCon 5,    parseScheme "forall a . forall b . forall c . forall d . forall e . a -> b -> c -> d -> e -> Tuple5 a b c d e")
  , (Name "show",   parseScheme "forall a . a -> List Char")
  , (Name "read",   parseScheme "forall a . List Char -> a")
  , (Name "+",      parseScheme "Int -> Int -> Int")
  , (Name "-",      parseScheme "Int -> Int -> Int")
  , (Name "*",      parseScheme "Int -> Int -> Int")
  , (Name "<=",     parseScheme "Int -> Int -> Bool")
  , (Name "<",      parseScheme "Int -> Int -> Bool")
  , (Name "==",     parseScheme "Int -> Int -> Bool")
  , (Name "/=",     parseScheme "Int -> Int -> Bool")
  , (Name ">",      parseScheme "Int -> Int -> Bool")
  , (Name ">=",     parseScheme "Int -> Int -> Bool")
  , (Name "||",     parseScheme "Bool -> Bool -> Bool")
  , (Name "&&",     parseScheme "Bool -> Bool -> Bool")
  , (Name "not",    parseScheme "Bool -> Bool")
  ]

-- |@freeInPrelude e@ returns the list of names, that appear free in @e@ and are 
-- not bound in the prelude
freeInPrelude :: Expression -> [Name]
freeInPrelude exp = free exp \\ (map fst $ toList prelude)
