{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module CO4.Test.ICFP.Tree.Solve
where

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude

$( compileFile [ImportPrelude, Cache] "CO4/Test/ICFP/Tree/Standalone.hs" )

uOp1 = constructors [ Just [], Just [], Just [], Just [], Just [] ]
uOp2 = constructors [ Just [], Just [], Just [], Just [] ]

-- fibonacci-like tree structure:

uE i | i <= 0 = constructors [ Just [], Just [], Just []
                    , Nothing, Nothing ]

uE i = constructors [ Just [], Just [], Just []
                    , Just [ uOp1, uE (i-1) ]
                    , Just [ uOp2, uE (i-1), uE (i-2) ]
                    ]

uP depth = constructors [ Just [ uE depth ] ]

allocator = uP 6

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
          Nothing -> return Nothing
          Just p  -> if constraint (map mapSample allSamples) p
                     then return $ Just $ pretty p
                     else iteration (numSamples + stepping) (stepping + 1)

-- * pretty printing
class Pretty a where
  pretty :: a -> String

instance Pretty P where
  pretty (P e) = concat ["(lambda ( x ) ", pretty e, ")"]

instance Pretty E where
  pretty Null           = "0"
  pretty One            = "1"
  pretty (Id)           = "x"
  pretty (Op1 op e)     = concat [ "(", pretty op, " ", pretty e, ")"]
  pretty (Op2 op e1 e2) = concat [ "(", pretty op, " "
                                   , pretty e1, " ", pretty e2, ")"]
  
instance Pretty Op1 where
  pretty Not   = "not"
  pretty Shl1  = "shl1"
  pretty Shr1  = "shr1"
  pretty Shr4  = "shr4"
  pretty Shr16 = "shr16"

instance Pretty Op2 where
  pretty And  = "and"
  pretty Or   = "or"
  pretty Xor  = "xor"
  pretty Plus = "plus"





{-- 
uId  = constructors [ Just [], Just [], Just [] ]
uOp1 = constructors [ Just [], Just [], Just [], Just [], Just [] ]
uOp2 = constructors [ Just [], Just [], Just [], Just [] ]

uE 0 = constructors [ Just [], Just [], Just [ uId ]
                    , Nothing, Nothing, Nothing, Nothing ]

uE i = constructors [ Just [], Just [], Just [ uId ]
                    , Just [ uE (i-1), uE (i-1), uE (i-1) ]
                    , Just [ uE (i-1), uE (i-1), uE (i-1) ]
                    , Just [ uOp1, uE (i-1) ]
                    , Just [ uOp2, uE (i-1), uE (i-1) ]
                    ]

uP depth = constructors [ Just [ uE depth ] ]
<<<<<<< HEAD
-}
