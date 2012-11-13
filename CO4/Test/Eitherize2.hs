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

{-# LANGUAGE UndecidableInstances #-}
module CO4.Test.Eitherize2
where

import           Prelude hiding (Maybe (..),Bool(..),and,not,(&&),(||),head,foldl,concat)
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

$([d| data Boolean = T | F 
      data Nat     = Z | S Nat
      -- data List a  = Nil | Cons a (List a)
      data NatList = Nil | Cons Nat (NatList)
      data Tree    = Leaf | Branch Tree Nat Tree  
      -- data Foo a  = Foo2 (List (Foo a))

      {-
      data MaybeNat = Nothing | Just Nat 

      maybeadd x y = case x of
        Nothing -> Nothing
        Just x' -> case y of
                    Nothing -> Nothing
                    Just y' -> Just  (add x' y')

      isJust x  = case x of
        Nothing -> F
        Just _   -> T

      main x = isJust ( maybeadd x (Nothing)) -- CO4/EncodedAdt.hs:(173,3)-(181,48): Non-exhaustive patterns in function select
      -}

      concat x y = case x of
        Nil -> y
        Cons x' xs -> Cons x' (concat xs y)

      inorder x = case x of
        Leaf -> Nil
        Branch l k r -> concat (inorder l) (Cons k (inorder r))

      listeq x y = case x of
        Nil -> case y of Nil -> T
                         Cons _ _ -> F
        Cons z zs -> case y of Nil -> F
                               Cons a as -> and (nateq z a) (listeq zs as)

      --main tree = listeq (inorder tree) (Cons (S Z) (Cons (S (S Z)) Nil))

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

      add x y = case x of
        Z -> y
        S x' -> S (add x' y)

      nateq x y = case x of
        Z -> case y of 
                Z -> T
                S _ -> F
        S x' -> case y of
                Z -> F
                S y' -> nateq x' y'

      main x = nateq (S (S Z)) (add x (S Z))  -- !!!!!!!!!!!

      monotone xs = case xs of
        Nil       -> F -- undefined
        Cons y ys -> case ys of
                        Nil      -> T
                        Cons z _ -> and (gt y z)
                                        (monotone ys)

      lengthOne xs = case xs of Nil -> F 
                                Cons y ys -> case ys of
                                              Nil -> T
                                              Cons z zs -> F

      --main xs = and (monotone xs) (lengthOne xs)

      --main x = gt x (S Z)
      --main xs = gt (at (S Z) xs) (at Z xs)

      --main xs = monotone xs
      --main xs = hasLength (S Z) xs
      --main xs = (monotone xs) 

   |] >>= \p -> TH.runIO $ compile p [ Verbose, NoRaml --,    NoSatchmo
                                     , DumpAll ""]
  )

--result = solve (undefined :: SizedTree Nat2 Nat5) encMain >>= putStrLn . show
result = solve (undefined :: SizedNat Nat5) encMain >>= putStrLn . show

deriving instance Show Boolean
--deriving instance Show MaybeNat
deriving instance Show Nat
deriving instance Show NatList
deriving instance Show Tree

{-
instance Unknown SizedBoolean where
  unknown _ = unknownConstructor [ Just encUnit, Just encUnit ]

instance Unknown (SizedNat Nat0) where
  unknown _ = unknownConstructor [ Just encUnit, Nothing ]

instance Unknown (SizedNat a) => Unknown (SizedNat (NatSucc a)) where
  unknown (_ :: SizedNat (NatSucc a)) = do
    u <- unknown (undefined :: (SizedNat a)) 
    unknownConstructor [ Just encUnit, Just $ encArgs [u] ]

instance Unknown a => Unknown (SizedList Nat0 a) where
  unknown _ = undefined

instance (Unknown a,Unknown (SizedList d a)) => Unknown (SizedList (NatSucc d) a) where
  unknown (_ :: SizedList (NatSucc d) a) = undefined

instance (Unknown a) => Unknown (SizedFoo Nat0 d a) where
  unknown (_ :: SizedFoo Nat0 d a) = do
    unknownConstructor [ Nothing ]

instance (Unknown a, Unknown (SizedList dl (SizedFoo df dl a) ))
      => Unknown (SizedFoo (NatSucc df) dl a) where
  unknown (_ :: SizedFoo (NatSucc df) dl a) = do
    u1 <- unknown (undefined :: SizedList dl (SizedFoo df dl a))
    unknownConstructor [ Just $ encArgs [u1] ]
-}

{-

instance (Unknown a,Unknown c) => Unknown (SizedTree a Nat0 c Nat0 Nat0) where
  unknown (_ :: SizedTree a Nat0 b Nat0 Nat0) = do
    a' <- unknown (undefined :: a)
    unknownConstructor [ Just $ encArgs [a'] , Nothing , Nothing]

instance (Unknown a,Unknown c,Unknown (SizedTree a Nat0 c d e)) 
  => Unknown (SizedTree a Nat0 c (NatSucc d) (NatSucc e)) where
    unknown (_ :: SizedTree a Nat0 c (NatSucc d) (NatSucc e)) = do
      a' <- unknown (undefined :: a)
      b' <- unknown (undefined :: SizedTree a Nat0 c d e)
      c' <- unknown (undefined :: c)
      d' <- unknown (undefined :: SizedTree a Nat0 c d e)
      e' <- unknown (undefined :: SizedTree a Nat0 c d e)
      unknownConstructor [ Just $ encArgs [a'] , Just $ encArgs [b',c',d'], Just $ encArgs [e'] ]

instance (Unknown a,Unknown c,Unknown (SizedTree a Nat0 c Nat0 e)) 
  => Unknown (SizedTree a Nat0 c Nat0 (NatSucc e)) where
    unknown (_ :: SizedTree a Nat0 c Nat0 (NatSucc e)) = do
      a' <- unknown (undefined :: a)
      b' <- unknown (undefined :: SizedTree a Nat0 c d e)
      c' <- unknown (undefined :: c)
      d' <- unknown (undefined :: SizedTree a Nat0 c d e)
      e' <- unknown (undefined :: SizedTree a Nat0 c d e)
      unknownConstructor [ Just $ encArgs [a'] , Just $ encArgs [b',c',d'], Just $ encArgs [e'] ]

instance (Unknown a,Unknown c,Unknown (SizedTree a b c Nat0 e)) 
  => Unknown (SizedTree a (NatSucc b) c Nat0 (NatSucc e)) where
    unknown (_ :: SizedTree a (NatSucc b) c Nat0 (NatSucc e)) = do
      a' <- unknown (undefined :: a)
      b' <- unknown (undefined :: SizedTree a b c Nat0 e)
      c' <- unknown (undefined :: c)
      d' <- unknown (undefined :: SizedTree a b c Nat0 e)
      e' <- unknown (undefined :: SizedTree a b c Nat0 e)
      unknownConstructor [ Just $ encArgs [a'] , Just $ encArgs [b',c',d'], Just $ encArgs [e'] ]

instance (Unknown a,Unknown c,Unknown (SizedTree a b c d e)) 
  => Unknown (SizedTree a (NatSucc b) c (NatSucc d) (NatSucc e)) where
    unknown (_ :: SizedTree a (NatSucc b) c (NatSucc d) (NatSucc e)) = do
      a' <- unknown (undefined :: a)
      b' <- unknown (undefined :: SizedTree a b c d e)
      c' <- unknown (undefined :: c)
      d' <- unknown (undefined :: SizedTree a b c d e)
      e' <- unknown (undefined :: SizedTree a b c d e)
      unknownConstructor [ Just $ encArgs [a'] , Just $ encArgs [b',c',d'], Just $ encArgs [e'] ]
      -}

