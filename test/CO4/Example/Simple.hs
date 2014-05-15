{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CO4.Example.Simple
where

import           Prelude (undefined,(>>=),error,Show (..),putStrLn,(.),id)
import qualified Data.Maybe as M
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4

$( [d| data Bool = False | True deriving Show

       not x = case x of False -> True
                         True  -> False

       and x y = case x of
        False -> False
        True  -> y

       constraint p x = and p (not x)

   |] >>= compile []
  )

allocator = complete

result = solveAndTestP True allocator encConstraint constraint
