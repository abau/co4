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
import           Language.Haskell.TH (runIO)
import qualified Satchmo.SAT.Mini 
import qualified Satchmo.Code 
import           CO4
import           CO4.Algorithms.Eitherize.UnsizedAdt (UnsizedAdt)

$([d|   

   data Bool   = False | True
   data List a = Nil | Cons a (List a)

   data Tree a = Leaf | Branch (Tree a) a (Tree a)

   data Rose a = Node a (List (Rose a))

   head xs = case xs of Nil -> undefined
                        Cons y ys -> y

   main r = case r of Node x xs -> 
                         case x of 
                          False -> False
                          True  -> True
   |] >>= runIO . configurable [Verbose] . compile 
 )

result = CO4.solve (undefined :: (SizedRose Nat2 Nat2 SizedBool)) encMain
