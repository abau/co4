{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module CO4.Test.Loop
where

import           Prelude (undefined,(>>=),error,Show (..),putStrLn,(.),(-),print)
import           Data.Maybe
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4


$( [d| 

        main d = looping_derivation n522 d

        -- rewriting system  ab -> bbaa.
        rs = RS (Cons (Rule (Cons A(Cons B (Cons B Nil)))
                            (Cons B(Cons B (Cons A (Cons A (Cons B Nil))))))
                  Nil)



        g03 = RS 
               (Cons (Rule (Cons A(Cons A(Cons A(Cons A Nil))))
                           (Cons A(Cons A(Cons B(Cons B Nil)))))
               (Cons (Rule (Cons B(Cons A(Cons A(Cons B Nil))))
                           (Cons A(Cons A(Cons B(Cons A Nil)))))
                     Nil))

        g08 = RS 
               (Cons (Rule (Cons A(Cons A(Cons A(Cons A Nil))))
                           (Cons A(Cons B(Cons B(Cons A Nil)))))
               (Cons (Rule (Cons B(Cons A(Cons A(Cons B Nil))))
                           (Cons A(Cons A(Cons B(Cons A Nil)))))
                     Nil))

        g10 = RS 
               (Cons (Rule (Cons A(Cons A(Cons A(Cons A Nil))))
                           (Cons A(Cons B(Cons B(Cons B Nil)))))
               (Cons (Rule (Cons B(Cons A(Cons A(Cons B Nil))))
                           (Cons A(Cons A(Cons B(Cons A Nil)))))
                     Nil))

        n11 = RS (Cons (Rule (Cons A Nil) Nil)
                 (Cons (Rule (Cons A Nil) (Cons B Nil))
                 (Cons (Rule (Cons A (Cons B (Cons C Nil))) 
                             (Cons C (Cons C (Cons B (Cons A (Cons A Nil))))))
                 (Cons (Rule (Cons C Nil) Nil)
                       Nil))))

        n515 = RS
               (Cons (Rule (Cons A(Cons A(Cons A Nil)))
                           (Cons B Nil))
               (Cons (Rule (Cons B(Cons C Nil))
                           (Cons C (Cons C (Cons A (Cons A (Cons A (Cons A Nil)))))))
                     Nil))

        n522 = RS (Cons (Rule (Cons A (Cons A (Cons B Nil)))
                              (Cons A (Cons C (Cons C (Cons A (Cons A (Cons A Nil)))))))
                  (Cons (Rule (Cons A (Cons C Nil)) (Cons B Nil))
                        Nil))
              

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

        data Sigma = A | B | C

        eqSigma x y = case x of
            A -> case y of
                A -> True
                B -> False
                C -> False
            B -> case y of
                A -> False
                B -> True
                C -> False
            C -> case y of
                A -> False
                B -> False
                C -> True

        data List a = Nil | Cons a (List a)

        null xs = case xs of
            Nil -> True
            Cons x xs -> False

        head xs = case xs of
            Nil -> undefined
            Cons x xs -> x

        last xs = case xs of
            Nil -> undefined
            Cons x xs -> case xs of
                Nil -> x
                Cons x ys -> last xs

        eqListSigma xs ys = case xs of
            Nil -> case ys of
                Nil -> True
                Cons y ys -> False
            Cons x xs -> case ys of
                Nil -> False
                Cons y ys -> 
                    and2 (eqSigma x y) (eqListSigma xs ys)

        append xs ys = case xs of
            Nil -> ys
            Cons x xs -> Cons x (append xs ys)

        reverse xs = rev_app xs Nil

        rev_app xs ys = case xs of
            Nil -> ys
            Cons x xs' -> rev_app xs' (Cons x ys)

        factor xs ys = or2 (prefix xs ys ) ( case ys of
            Nil -> False
            Cons y ys -> factor xs ys )

        prefix xs ys = case xs of
            Nil -> True
            Cons x xs -> case ys of
                Nil -> False
                Cons y ys -> 
                    and2 (eqSigma x y) (prefix xs ys)

        -- this is tricky (can we do it in linear size, as we do for prefix?)
        suffix xs ys = prefix (reverse xs) (reverse ys)
{-
        or2 (eqListSigma xs ys) ( case ys of
            Nil -> False
            Cons y ys' -> suffix xs ys' )
-}


        foldr f z xs = case xs of
            Nil -> z
            Cons x xs -> f x (foldr f z xs)

        map f xs = foldr ( \ x y -> Cons (f x) y ) Nil xs

        or  xs = foldr or2 False xs
        and xs = foldr and2 True xs

        forall xs f = and ( map f xs )
        exists xs f = or  ( map f xs )

        data Rule = Rule (List Sigma) (List Sigma)

        eqRule u1 u2 = case u1 of
                Rule l1 r1 -> case u2 of
                    Rule l2 r2 -> 
                        and2 (eqListSigma l1 l2)
                             (eqListSigma r1 r2)

        data RS = RS (List Rule)

        data Step = Step (List Sigma) -- prefix
                         Rule
                         (List Sigma) -- suffix

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
                Step p u s -> null p

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
                Rule l r -> append p (append l s)

        right_semantics step = case step of
            Step p u s -> case u of
                Rule l r -> append p (append r s)

        derivation_is_looping d = 
              -- factor (left_semantics (head d)) (right_semantics (last d))
              suffix (left_semantics (head d)) (right_semantics (last d))

        joinable_steps step1 step2 = 
            -- eqListSigma (right_semantics step1) (left_semantics  step2)
            -- factor (left_semantics  step2) (right_semantics step1) 
            prefix (left_semantics  step2) (right_semantics step1) 

        step_uses_rules rules step = case step of
           Step p u s -> 
               exists rules ( \ v -> eqRule u v )

   |] >>= runIO . configurable [Verbose, Profiling, DumpAll "/tmp/Loop" ] . compile 
  )

uBool      = constructors [ Just [] , Just [] ]
uSigma     = constructors [ Just [] , Just [], Just [] ]
uList 0 _  = constructors [ Just [] , Nothing ]
uList i a  = constructors [ Just [] , Just [a, uList (i-1) a ] ]

uRule wordLength = constructors [ Just [ uList wordLength uSigma
                                       , uList wordLength uSigma ] ]

kNil       = known 0 2 []
kCons x xs = known 1 2 [ x , xs ]
kList 0 _  = kNil
kList i a  = kCons a (kList (i-1) a)

kRule wordLength = known 0 1 [ kList wordLength uSigma
                             , kList wordLength uSigma
                             ]

uStep  rw w = known 0 1 [ uList w uSigma
                        , uRule rw
                        , uList w uSigma
                        ]

kStep  rw w = known 0 1 [ uList w uSigma
                        , kRule rw
                        , uList w uSigma
                        ]

allocator rw w l = ( uList l (uStep rw w))

allokator rw w l = ( uList l (kStep rw w))

result = solveAndTestBoolean (allocator 6 20 20)  encMain main


