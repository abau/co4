{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CO4.Test.Simple
where

import           Prelude (undefined,(>>=),error,Show (..),putStrLn,(.))
import           Language.Haskell.TH (runIO)
import qualified Satchmo.SAT.Mini 
import qualified Satchmo.Code 
import           CO4

$( [d| data Bool = False | True

       main x = case x of False -> False
                          True  -> True

   |] >>= runIO . configurable [Verbose] . compile 
  )

result = solve (undefined :: SizedBool) encMain >>= putStrLn . show
