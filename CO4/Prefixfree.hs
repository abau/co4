module CO4.Prefixfree
  (numeric, invNumeric)
where

import qualified Control.Exception as Exception
        
numeric :: Integer -> [Bool] -> Integer
numeric n xs = go 0 n xs
  where
    go sum 1 _  = sum
    go sum n xs = case xs of 
      (False : ys) -> go  sum      c ys
      (True  : ys) -> go (sum + c) f ys
      []           -> error "Prefixfree.numeric: not enough flags"
      where 
        c = ceiling $ fromInteger n / 2
        f = floor   $ fromInteger n / 2

invNumeric :: Integer -> Integer -> [Bool]
invNumeric n i = Exception.assert (i < n) 
               $ go [] n i
  where
    go flags 1 0 = flags
    go flags n i = if i < c 
                   then go (flags ++ [False]) c i
                   else go (flags ++ [True]) f $ i - c
      where 
        c = ceiling $ fromInteger n / 2
        f = floor   $ fromInteger n / 2
