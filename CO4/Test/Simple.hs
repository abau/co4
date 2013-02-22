{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module CO4.Test.Simple
where

import Prelude (undefined,(>>=),error,Show (..),putStrLn,(.))
import Language.Haskell.TH (runIO)
import Satchmo.SAT.Mini (SAT)
import Satchmo.Code (Decode,decode)
import CO4

$( [d| data Bool = False | True

       main x = case x of False -> False
                          True  -> True

   |] >>= \p -> runIO (compile p [Verbose, NoRaml])
  )

result = solve (undefined :: SizedBool) encMain >>= putStrLn . show

deriving instance Show Bool
