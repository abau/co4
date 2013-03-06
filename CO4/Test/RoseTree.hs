{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

module CO4.Test.RoseTree
where

import           Prelude (undefined,(>>=),error,Show (..),putStrLn,(.))
import qualified GHC.Types
import           Language.Haskell.TH (runIO)
import qualified Satchmo.SAT.Mini 
import qualified Satchmo.Code 
import           CO4
import           CO4.Algorithms.Eitherize.UnsizedAdt (UnsizedAdt)

$([d|   

   data Bool   = False | True
   data List a = Nil | Cons a (List a)

   data Rose a = Node a (List (Rose a))

   main r = case r of Node x xs -> 
                         case x of 
                          False -> False
                          True  -> True

   |] >>= runIO . configurable [Verbose,DumpAll ""] . compile 
 )

result = CO4.solve (undefined :: (SizedRose Nat2 Nat2 SizedBool)) encMain

