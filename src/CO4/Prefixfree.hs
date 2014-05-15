module CO4.Prefixfree
  (numeric, invNumeric, discriminates)
where

import qualified Control.Exception as Exception
import           Satchmo.Core.Primitive (Primitive,isConstant)
import           CO4.Util (bitWidth)

numeric :: Integral i => i -> [Bool] -> i
numeric n' xs' = go 0 n' xs'
  where
    go sum 1 _  = sum
    go sum n xs = case xs of 
      (False : ys) -> go  sum      c ys
      (True  : ys) -> go (sum + c) f ys
      []           -> error $ "Prefixfree.numeric: not enough flags"
      where 
        c = ceiling $ fromIntegral n / 2
        f = floor   $ fromIntegral n / 2

invNumeric :: Integral i => i -> i -> [Bool]
invNumeric n i = Exception.assert (i < n) 
               $ go [] n i
  where
    go flags 1 0 = flags
    go flags n i = if i < c 
                   then go (flags ++ [False]) c i
                   else go (flags ++ [True]) f $ i - c
      where 
        c = ceiling $ fromIntegral n / 2
        f = floor   $ fromIntegral n / 2

-- |@discriminates n xs@ checks whether @xs@ can 
-- discriminate @n@ different states
discriminates :: (Primitive p, Integral i) => i -> [p] -> Bool
discriminates n xs = case all isConstant xs of
  False -> length xs >= w
  True  -> length xs >= w - 1
  where
    w = bitWidth n
