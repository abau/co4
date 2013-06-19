{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module CO4.Example.Simple
where

import           Prelude (undefined,(>>=),error,Show (..),putStrLn,(.),id)
import qualified Data.Maybe as M
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4

$( [d| data Bool = False | True

       not x = case x of False -> True
                         True  -> False

       and x y = case x of
        False -> False
        True  -> y

       main p x = and p (not x)

   |] >>= runIO . configurable [] . compile 
  )

allocator = constructors [ M.Just [] , M.Just [] ]

result = solveAndTestBooleanP True id allocator encMain main 
