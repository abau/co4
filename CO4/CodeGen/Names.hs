{-# LANGUAGE LambdaCase #-}
module CO4.CodeGen.Names
  (encodedConsName, encodedName, encodedNameProf, allocatorName)
where

import Data.Char (toUpper)
import CO4.Names 

encodedConsName :: Namelike a => a -> a
encodedConsName = mapName (\(n:ns) -> "enc" ++ (toUpper n : ns) ++ "Cons") 

encodedName :: Namelike a => a -> a
encodedName = mapName (\case "&&"   -> "encAnd2"
                             "||"   -> "encOr2"
                             (n:ns) -> "enc" ++ (toUpper n : ns))

encodedNameProf :: Namelike a => a -> a
encodedNameProf = mapName (\case 
  n | n == mainName           -> encodedName n
  n | n == deprecatedMainName -> encodedName n
  n                           -> encodedName n ++ "Prof")

allocatorName :: Namelike a => a -> a
allocatorName = mapName (\(n:ns) -> "alloc" ++ (toUpper n : ns)) 

