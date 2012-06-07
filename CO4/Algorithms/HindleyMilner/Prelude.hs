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
  [ (untypedName consCon,         parseScheme "forall a . a -> List a -> List a")
  , (untypedName nilCon,          parseScheme "forall a . List a")
  , (untypedName trueCon,         SType $ TCon boolType [])
  , (untypedName falseCon,        SType $ TCon boolType [])
  , (untypedName $ tupleCon 2,    parseScheme "forall a . forall b . a -> b -> Tuple2 a b")
  , (untypedName $ tupleCon 3,    parseScheme "forall a . forall b . forall c . a -> b -> c -> Tuple3 a b c")
  , (untypedName $ tupleCon 4,    parseScheme "forall a . forall b . forall c . forall d . a -> b -> c -> d -> Tuple4 a b c d")
  , (untypedName $ tupleCon 5,    parseScheme "forall a . forall b . forall c . forall d . forall e . a -> b -> c -> d -> e -> Tuple5 a b c d e")
  , (untypedName "show",          parseScheme "forall a . a -> List Char")
  , (untypedName "read",          parseScheme "forall a . List Char -> a")
  , (untypedName "+",             parseScheme "Int -> Int -> Int")
  , (untypedName "-",             parseScheme "Int -> Int -> Int")
  , (untypedName "*",             parseScheme "Int -> Int -> Int")
  , (untypedName "<",             parseScheme "Int -> Int -> Bool")
  , (untypedName "<=",            parseScheme "Int -> Int -> Bool")
  , (untypedName "==",            parseScheme "Int -> Int -> Bool")
  , (untypedName "/=",            parseScheme "Int -> Int -> Bool")
  , (untypedName ">=",            parseScheme "Int -> Int -> Bool")
  , (untypedName ">",             parseScheme "Int -> Int -> Bool")
  , (untypedName "||",            parseScheme "Bool -> Bool -> Bool")
  , (untypedName "&&",            parseScheme "Bool -> Bool -> Bool")
  , (untypedName "not",           parseScheme "Bool -> Bool")
  ]

-- |@freeInPrelude e@ returns the list of names, that appear free in @e@ and are 
-- not bound in the prelude
freeInPrelude :: Expression -> [Name]
freeInPrelude exp = free exp \\ (map (name . fst) $ toList prelude)
