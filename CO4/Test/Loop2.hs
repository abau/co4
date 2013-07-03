{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module CO4.Test.Loop2
where

import           Prelude (undefined,(>>=),error,Show (..),putStrLn,(.),(-),print)
import           Data.Maybe
import qualified GHC.Types
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4


$( [d| 

        main d = looping_derivation g03 d

        data Bool = False | True

        or2 x y = case x of
            False -> y
            True  -> x

        and2 x y = case x of
            False -> x
            True  -> y

        not x  = case x of
            False -> True
            True -> False

        data Word = E | A Word | B Word | C Word

        nullW xs = case xs of 
            E -> True ; A x -> False ; B x -> False ; C x -> False

        appendW xs ys = case xs of
            E -> ys
            A xs' -> A (appendW xs' ys)
            B xs' -> B (appendW xs' ys)
            C xs' -> C (appendW xs' ys)

        eqW xs ys = case xs of
            E -> case ys of
                E -> True ; A w -> False ; B w -> False ; C w -> False
            A v -> case ys of
                E -> False ; A w -> eqW v w ; B w -> False ; C w -> False
            B v -> case ys of
                E -> False ; A w -> False ; B w -> eqW v w ; C w -> False
            C v -> case ys of
                E -> False ; A w -> False ; B w -> False ; C w -> eqW v w 

        reverseW xs = rev_app xs E

        rev_app xs ys = case xs of
            E -> ys
            A xs' -> rev_app xs' (A ys)
            B xs' -> rev_app xs' (B ys)
            C xs' -> rev_app xs' (C ys)

        prefixW xs ys = case xs of
            E -> True
            A xs -> case ys of
                E -> False; A ys -> prefixW xs ys ; B w -> False ; C w -> False
            B xs -> case ys of
                E -> False; A w -> False ; B ys -> prefixW xs ys ; C w -> False
            C xs -> case ys of
                E -> False; A w -> False ; B w -> False ; C ys -> prefixW xs ys

        -- this is tricky (can we do it in linear size, as we do for prefixW?)
        suffixW xs ys = prefixW (reverseW xs) (reverseW ys)


        g03 = RS (Cons (Rule (A (A (A (A E)))) (A (A (B (B E)))))
                 (Cons (Rule (B (A (A (B E)))) (A (A (B (A E))))) Nil))



        data List a = Nil | Cons a (List a)

        null xs = case xs of Nil -> True ; Cons x xs ->  False
        head xs = case xs of Nil -> undefined ; Cons x xs ->  x
        last xs = case xs of 
            Nil -> undefined
            Cons x xs -> case xs of Nil -> x ; Cons y ys -> last xs

        foldr f z xs = case xs of
            Nil -> z
            Cons x xs -> f x (foldr f z xs)

        map f xs = foldr ( \ x y -> Cons (f x) y ) Nil xs

        or  xs = foldr or2 False xs
        and xs = foldr and2 True xs

        forall xs f = and ( map f xs )
        exists xs f = or  ( map f xs )


        data Rule = Rule Word Word

        eqRule u1 u2 = case u1 of
                Rule l1 r1 -> case u2 of
                    Rule l2 r2 -> 
                        and2 (eqW l1 l2)
                             (eqW r1 r2)

        data RS = RS (List Rule)

        data Step = Step Word -- prefix
                         Rule
                         Word -- suffix

        -- type Derivation = List Step

        looping_derivation rs d =
           and2  True -- (break_symmetry d)
            ( and2 (derivation_is_nonempty d)
             ( and2 (derivation_uses_rules rs d)
              (and2 (derivation_is_joinable d)
                   (derivation_is_looping d))))

        break_symmetry d = case d of
            Nil -> False
            Cons s later -> case s of
                Step p u s -> nullW p

        derivation_uses_rules rs d = case rs of
            RS rules -> forall d
                ( \ s -> step_uses_rules rules s )

        derivation_is_nonempty d = not (null d)

        derivation_is_joinable d = case d of
            Nil -> True
            Cons s1 later1 -> case later1 of
                Nil -> True
                Cons s2 later2 -> 
                    and2 (joinable_steps s1 s2)
                         (derivation_is_joinable later1)

        -- TODO: this is what I want to write:
        -- right_semantics (Step p (Rule l r) s) = 
        --    append p (append r s)

        left_semantics step = case step of
            Step p u s -> case u of
                Rule l r -> appendW p (appendW l s)

        right_semantics step = case step of
            Step p u s -> case u of
                Rule l r -> appendW p (appendW r s)

        derivation_is_looping d = 
              -- factor (left_semantics (head d)) (right_semantics (last d))
              suffixW (left_semantics (head d)) (right_semantics (last d))

        joinable_steps step1 step2 = 
            -- eqW (right_semantics step1) (left_semantics  step2)
            -- factor (left_semantics  step2) (right_semantics step1) 
            prefixW (left_semantics  step2) (right_semantics step1) 

        step_uses_rules rules step = case step of
           Step p u s -> 
               exists rules ( \ v -> eqRule u v )

   |] >>= runIO . configurable [Verbose, DumpAll "/tmp/Loop" ] . compile 
  )

uBool      = constructors [ Just [] , Just [] ]

uList 0 _  = constructors [ Just [] , Nothing ]
uList i a  = constructors [ Just [] , Just [a, uList (i-1) a ] ]

uWord 0 = constructors [ Just [] , Nothing, Nothing, Nothing ]
uWord i = constructors 
      [ Just [] , Just [ uWord (i-1)], Just [ uWord (i-1)], Just [ uWord (i-1)] ]

uRule wordLength = constructors [ Just [ uWord wordLength 
                                       , uWord wordLength ] ]

uStep  rw w = known 0 1 [ uWord w 
                        , uRule rw
                        , uWord w 
                        ]

allocator rw w l = ( uList l (uStep rw w))

result = solveAndTest (allocator 4 10 20)  encMain main


