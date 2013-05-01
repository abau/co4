{-# LANGUAGE TemplateHaskell #-}
module CO4.Prelude
  (prelude, parsePrelude)
where

import           Prelude ((>>=),(.),return,Show)
import qualified Prelude as P
import           Control.Monad.IO.Class (MonadIO)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (Quasi)
import           CO4.Language (Declaration)
import           CO4.Unique (MonadUnique)
import           CO4.Frontend.TH (parseTHDeclaration)
import           CO4.Frontend.THPreprocess (preprocess,eraseDerivings)

parsePrelude :: (Quasi m, MonadIO m, MonadUnique m) => m [Declaration]
parsePrelude = TH.runQ prelude 
           >>= preprocess . eraseDerivings
           >>= return . P.map parseTHDeclaration 

prelude :: TH.Q [TH.Dec]
prelude = 
  [d| 
      -- Functional -----------------------------

      const x _ = x

      -- Maybe ----------------------------------

      data Maybe a = Nothing | Just a deriving Show

      fromMaybe a' m = case m of
        Nothing -> a'
        Just a  -> a

      -- Either ---------------------------------

      data Either a b = Left a | Right b deriving Show

      -- Pair -----------------------------------

      data Pair a b = Pair a b deriving Show

      fst (Pair a _) = a
      snd (Pair _ b) = b

      -- Lists ----------------------------------

      data List a = Nil | Cons a (List a) deriving Show

      map f xs = case xs of
        Nil       -> Nil
        Cons y ys -> Cons (f y) (map f ys)

      fold cons nil xs = case xs of
        Nil       -> nil
        Cons y ys -> cons y (fold cons nil ys)

      head xs = case xs of 
        Nil      -> Nothing
        Cons x _ -> Just x

      append xs ys = fold Cons ys xs

      -- Booleans -------------------------------

      data Bool = False | True deriving Show

      not x = case x of
        False -> True
        True  -> False

      or2 x y = case x of
        False -> y
        True  -> True

      and2 x y = case x of
        False -> False
        True  -> y

      and         = fold and2 True
      or          = fold or2  False
      forall xs f = and ( map f xs )
      exists xs f = or  ( map f xs )

      -- Unary numbers --------------------------

      data Unary = UZero | USucc Unary

      uZero  = UZero
      uOne   = USucc uZero
      uTwo   = USucc uOne
      uThree = USucc uTwo
      uFour  = USucc uThree
      uFive  = USucc uFour
      uSix   = USucc uFive
      uSeven = USucc uSix
      uEight = USucc uSeven
      uNine  = USucc uEight
      uTen   = USucc uNine

      eqUnary x y = case x of
        UZero    -> case y of UZero    -> True
                              _        -> False
        USucc x' -> case y of UZero    -> False
                              USucc y' -> eqUnary x' y'

      -- Various --------------------------------

      length = fold (\_ u -> USucc u) UZero 
  |]
