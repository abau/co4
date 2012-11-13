{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module CO4.Test.Eitherize3
where

import           Prelude hiding (Maybe (..),length,Bool(..),and,not,(&&),(||),head,foldl,concat)
import           Control.Applicative ((<$>))
import qualified Language.Haskell.TH as TH
import           Satchmo.SAT.Mini (SAT)
import           Satchmo.Code (Decode,decode)
import           CO4
import           CO4.EncodedAdt
import           CO4.Algorithms.Eitherize.SizedGadt
import           CO4.Algorithms.Eitherize.UnknownGadtInstance
import           CO4.Algorithms.Eitherize.Solve (solve)
import           CO4.Algorithms.Eitherize.Util
import           CO4.Algorithms.Eitherize.DecodedAdtTypeFamily (DecodedAdt)

$([d| data Boolean = T | F 
      data Nat     = Z | S Nat
      data List a  = Nil | Cons a (List a)

      and x y = case x of
        T -> case y of
              T -> T
              F -> F
        F -> F

      gt x y = case x of
        Z    -> F
        S x' -> case y of
                  Z    -> T
                  S y' -> gt x' y'

      monotone xs = case xs of
        Nil       -> F 
        Cons y ys -> case ys of
                        Nil      -> T
                        Cons z _ -> and (gt y z) (monotone ys)

      length n xs = case n of
        Z    -> case xs of Nil       -> T
                           Cons _ _  -> F
        S n' -> case xs of Nil       -> F
                           Cons _ ys -> length n' ys

      main xs = and ( length (S ( S ( S Z))) xs )
                    (        monotone xs        )

   |] >>= \p -> TH.runIO $ compile p [ Verbose, NoRaml, DumpAll ""]
  )

result = solve (undefined :: SizedList Nat5 (SizedNat Nat5)) encMain >>= putStrLn . show

deriving instance Show Boolean
deriving instance Show Nat
deriving instance Show a => Show (List a)
