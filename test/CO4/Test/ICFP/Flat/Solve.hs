{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Test.ICFP.Flat.Solve
where

import           Control.Exception (assert)
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Util (bitWidth)

$( compileFile [ImportPrelude,Cache] "CO4/Test/ICFP/Flat/Standalone.hs" )

uOp1 = constructors [ Just [], Just [], Just [], Just [], Just [] ]
uOp2 = constructors [ Just [], Just [], Just [], Just [] ]

uPtr = uNat pointerSize

uE = constructors [ Just [], Just [], Just [ ]
                  , Just [ uPtr, uPtr, uPtr ]
                  , Just [ uOp1, uPtr ]
                  , Just [ uOp2, uPtr, uPtr ]
                  ]

uP = constructors [ Just [ uPtr ] ]

uMemory = kList (2^pointerSize) uE

allocator = uTuple2 uMemory uP 

pointerSize = 3     -- > also change in Standalone.{nullNat,incrementNat,eval}

-- | progressively increase the number of samples
result :: [(Integer,Integer)] -> IO (Maybe String)
result allSamples = iteration 2 1
  where
    iteration numSamples stepping = 
      let mapSample (x,y) = (nat 64 x, nat 64 y)
          samples'        = map mapSample $ take numSamples allSamples
      in 
        solveAndTestP samples' allocator encConstraint constraint
        >>= \case
          Nothing    -> return Nothing
          Just (m,p) -> 
            let m' = redirectForwardPointers m
            in
              if           constraint (map mapSample allSamples) (m ,p)
              then assert (constraint (map mapSample allSamples) (m',p))
                 $ return $ Just $ pretty p m'
              else iteration (numSamples + stepping) (stepping + 1)

redirectForwardPointers :: [E] -> [E]
redirectForwardPointers = 
  snd . foldl (\(ptr,memory) m -> (incrementNat ptr, memory ++ [go ptr m]))
              (nat pointerSize 0, []) 
  where
    go ptr (If a b c)   = If (redirect ptr a) (redirect ptr b) (redirect ptr c)
    go ptr (Op1 op a)   = Op1 op (redirect ptr a)
    go ptr (Op2 op a b) = Op2 op (redirect ptr a) (redirect ptr b)
    go _   e            = e

    redirect ptr p = if isForwardPointer ptr p
                     then nat pointerSize 0
                     else p

    isForwardPointer ptr _ | isZeroNat ptr = error "head of memory must not contain pointers"
    isForwardPointer ptr p = geNat p ptr

    pointers (If a b c)  = [a,b,c]
    pointers (Op1 _ a)   = [a]
    pointers (Op2 _ a b) = [a,b]
    pointers _           = []

-- * pretty printing
class Pretty a where
  pretty :: a -> [E] -> String

instance Pretty Nat where 
  pretty ptr memory = pretty (at ptr memory) memory

instance Pretty P where
  pretty (P e) m = concat ["(lambda ( x ) ", pretty e m, ")"]

instance Pretty E where
  pretty Null           _ = "0"
  pretty One            _ = "1"
  pretty (Id)           _ = "x"
  pretty (If c t f)     m = concat [ "(if0" , " ", pretty c m
                                            , " ", pretty t m
                                            , " ", pretty f m
                                            , ")"]
  pretty (Op1 op e)     m = concat [ "(", pretty op m, " ", pretty e m, ")"]
  pretty (Op2 op e1 e2) m = concat [ "(", pretty op m, " "
                                   , pretty e1 m, " ", pretty e2 m, ")"]
  
instance Pretty Op1 where
  pretty Not   _ = "not"
  pretty Shl1  _ = "shl1"
  pretty Shr1  _ = "shr1"
  pretty Shr4  _ = "shr4"
  pretty Shr16 _ = "shr16"

instance Pretty Op2 where
  pretty And  _ = "and"
  pretty Or   _ = "or"
  pretty Xor  _ = "xor"
  pretty Plus _ = "plus"

