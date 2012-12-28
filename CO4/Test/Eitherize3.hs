{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module CO4.Test.Eitherize3
where

import           Prelude hiding (Either (..),Maybe (..),length,Bool(..),and,not,(&&),(||),head,foldl,concat)
import           Control.Applicative ((<$>))
import qualified Language.Haskell.TH as TH
import           Satchmo.SAT.Mini (SAT)
import           Satchmo.Code (Decode,decode)
import           CO4

-- Reexport type families ???
import           CO4.Algorithms.Eitherize.DecodedAdtTypeFamily (DecodedAdt)

$([d| data Bool    = T | F 
      data Nat     = Z | S Nat
      data List a  = Nil | Cons a (List a)

      data Pair a b = Pair a b
      data Either a b = Left a | Right b

      and x y = case x of
        T -> case y of
              T -> T
              F -> F
        F -> F

      not x = case x of T -> F
                        F -> T

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


      -- main xs = and ( length (S ( S ( S Z))) xs )
      --               (        monotone xs        )

      main x = gt x ( S Z ) -- SizedNat Nat2

   |] >>= \p -> TH.runIO $ compile p [ Verbose, NoRaml, DumpAll ""]
  )

--result = solve (undefined :: SizedEither SizedBool (SizedPair SizedBool SizedBool)) encMain >>= putStrLn . show
result = solve (undefined :: SizedNat Nat2) encMain >>= putStrLn . show

deriving instance Show Bool
deriving instance Show Nat
deriving instance Show a => Show (List a)
deriving instance (Show a, Show b) => Show (Pair a b)
deriving instance (Show a, Show b) => Show (Either a b)
