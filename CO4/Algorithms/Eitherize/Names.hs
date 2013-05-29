module CO4.Algorithms.Eitherize.Names
  (encodedConsName, encodedName, allocatorName)
where

import Data.Char (toUpper)
import CO4.Names 

encodedConsName :: Namelike a => a -> a
encodedConsName = mapName (\(n:ns) -> "enc" ++ (toUpper n : ns) ++ "Cons") 

encodedName :: Namelike a => a -> a
encodedName = mapName (\(n:ns) -> "enc" ++ (toUpper n : ns)) 

allocatorName :: Namelike a => a -> a
allocatorName = mapName (\(n:ns) -> "alloc" ++ (toUpper n : ns)) 

