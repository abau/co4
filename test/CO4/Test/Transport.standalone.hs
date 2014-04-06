module CO4.Test.Transport where
import qualified Prelude 
import           Prelude (undefined)


-- looping transport system, see "Lindenmayer Loops"
-- http://www.imn.htwk-leipzig.de/~waldmann/talk/07/ajrw/



main ts = transport_system hw1 ts
test_main = main hw1t


-- Geser's system R_2 (slide 24)
r2 = RS (Cons (Rule (Cons B (Cons A (Cons A Nil)))
                    (Cons A(Cons A (Cons B(Cons B(Cons A Nil)))))) Nil)

-- HofWald1 (slide 22)
hw1 = RS (Cons hw_r1 (Cons hw_r2 Nil))

hw_r1 =  (Rule (Cons C(Cons B Nil)) (Cons B(Cons B(Cons A Nil))))
hw_r2 =  (Rule (Cons A(Cons B Nil)) (Cons B(Cons C(Cons A Nil))))

s11a3n1 = RS
    (Cons (Rule (Cons A Nil) Nil)
    (Cons (Rule (Cons A(Cons B Nil)) (Cons C Nil))
    (Cons (Rule (Cons C(Cons C Nil)) (Cons B(Cons C(Cons B(Cons A(Cons A Nil))))))
          Nil)))

s11a3n15 = RS
    (Cons (Rule (Cons A(Cons C Nil)) Nil)
    (Cons (Rule (Cons A(Cons A (Cons B Nil))) 
                (Cons B(Cons C(Cons B(Cons A(Cons A (Cons A Nil)))))))
          Nil))


data Move = Move (List Sigma) -- ^ origin (block letter)
                 (List (List Sigma)) -- ^ image (concatenation of block letters)
                 (List Step)  -- ^ origin . pivot ->> pivot . image
    -- deriving Prelude.Show

-- type Morphism = (List Move)

data Transport = Transport (List Sigma) -- ^ pivot
                           (List Move)  -- ^ morphism
                           (List Sigma) -- ^ start
                           (List Image)
    -- deriving Prelude.Show

data Image = Image (List (List Sigma)) -- ^  phi^k (start)
                   ( List (List Sigma)) -- ^  start ^ pivot^k
    -- deriving Prelude.Show

-- transport system for hw1

hw1t :: Transport
hw1t = Transport (Cons B Nil) 
                 hw1m
                 (Cons A Nil)
                 hw1i

hw1i = -- api 5 hw1m (Cons B Nil)  (Cons A Nil) 
   Cons (Image (Cons (Cons B Nil) (Cons (Cons B Nil) (Cons (Cons C Nil) (Cons (Cons A Nil) (Cons (Cons B Nil) (Cons (Cons A Nil) (Cons (Cons C Nil) (Cons (Cons A Nil) (Cons (Cons B Nil) (Cons (Cons B Nil) (Cons (Cons A Nil) (Cons (Cons C Nil) (Cons (Cons A Nil) (Cons (Cons B Nil) (Cons (Cons C Nil) (Cons (Cons A Nil) (Cons (Cons B Nil) (Cons (Cons A Nil) (Cons (Cons C Nil) (Cons (Cons A Nil) Nil)))))))))))))))))))) (Cons (Cons A Nil) (Cons (Cons B Nil) (Cons (Cons B Nil) (Cons (Cons B Nil) (Cons (Cons B Nil) (Cons (Cons B Nil) Nil))))))) (Cons (Image (Cons (Cons B Nil) (Cons (Cons B Nil) (Cons (Cons A Nil) (Cons (Cons C Nil) (Cons (Cons A Nil) (Cons (Cons B Nil) (Cons (Cons C Nil) (Cons (Cons A Nil) (Cons (Cons B Nil) (Cons (Cons A Nil) (Cons (Cons C Nil) (Cons (Cons A Nil) Nil)))))))))))) (Cons (Cons A Nil) (Cons (Cons B Nil) (Cons (Cons B Nil) (Cons (Cons B Nil) (Cons (Cons B Nil) Nil)))))) (Cons (Image (Cons (Cons B Nil) (Cons (Cons C Nil) (Cons (Cons A Nil) (Cons (Cons B Nil) (Cons (Cons A Nil) (Cons (Cons C Nil) (Cons (Cons A Nil) Nil))))))) (Cons (Cons A Nil) (Cons (Cons B Nil) (Cons (Cons B Nil) (Cons (Cons B Nil) Nil))))) (Cons (Image (Cons (Cons B Nil) (Cons (Cons A Nil) (Cons (Cons C Nil) (Cons (Cons A Nil) Nil)))) (Cons (Cons A Nil) (Cons (Cons B Nil) (Cons (Cons B Nil) Nil)))) (Cons (Image (Cons (Cons C Nil) (Cons (Cons A Nil) Nil)) (Cons (Cons A Nil) (Cons (Cons B Nil) Nil))) (Cons (Image (Cons (Cons A Nil) Nil) (Cons (Cons A Nil) Nil)) Nil)))))


hw1m =                  (Cons (Move (Cons A Nil)
                             (Cons (Cons C Nil) (Cons (Cons A Nil) Nil))
                             (Cons (Step Nil hw_r2 Nil  ) Nil ))
                    (Cons (Move (Cons C Nil)
                             (Cons (Cons B Nil) (Cons (Cons A Nil) Nil))
                             (Cons (Step Nil hw_r1 Nil) Nil))
                      (Cons (Move (Cons B Nil) (Cons (Cons B Nil) Nil) Nil) Nil)))

{-
api k m p s = if k Prelude.== 0 then Cons (Image (Cons s Nil) (Cons s Nil)) Nil
   else let c = api (k Prelude.- 1) m p s 
            Cons i  x = c
            Image u v = i
        in  Cons (Image (apply m u) (append v (Cons p Nil))) c
-}

transport_system r ts = case ts of 
    Transport pivot morphism start images -> 
        and2 (nontrivial start morphism)
      (  and2 (morphism_ok pivot r morphism) 
        ( and2 (front_embedding images)
          ( images_ok pivot morphism start images ) ) )

front_embedding images = case images of
    Nil -> False
    Cons i is -> case is of
        Nil -> False
        Cons j js -> case i of
            Image w s -> subword s w

-- | this should be cached!
subword s w = case s of
    Nil -> True
    Cons x xs -> case w of
        Nil -> False
        Cons y ys -> case eqListSigma x y of
            False -> subword s ys
            True -> subword xs ys

images_ok pivot morphism start images = 
    case images of
        Nil -> False
        Cons i is -> case i of
            Image w s -> case is of
                Nil -> and2 (eqListListSigma w (Cons start Nil)) 
                            (eqListListSigma s (Cons start Nil))
                Cons j js -> case j of
                    Image ww ss -> 
                        and2 ( eqListListSigma w (apply morphism ww) )
                      ( and2 ( eqListListSigma s (append ss (Cons pivot Nil)))
                             ( images_ok pivot morphism start is ) )

apply morphism w = concat ( map (apply_to_letter morphism) w )

apply_to_letter morphism x = case morphism of
    Nil -> undefined
    Cons m ms -> case m of
        Move orig imag derive -> case eqListSigma orig x of
            False -> apply_to_letter ms x
            True  -> imag

                    
eqListListSigma xs ys = case xs of
    Nil -> case ys of
        Nil -> True
        Cons y ys -> False
    Cons x xs -> case ys of
        Nil -> False
        Cons y ys -> and2 (eqListSigma x y) (eqListListSigma xs ys)


nontrivial start morphism = case morphism of
    Nil -> False
    Cons move moves -> case move of
        Move orig imag derive -> 
             and2 (eqListSigma start orig) (not (null derive))

morphism_ok p r morph = 
   and2 (forall morph ( move_ok p r ))
        (forall morph (blocks_ok morph))

blocks_ok morph move = case move of
    Move orig imag derive -> 
        forall imag ( \ block -> exists morph ( \ m -> case m of
            Move o i d -> eqListSigma o block ) )

move_ok p r move = case move of
    Move orig imag derive -> and2 (not (null orig)) ( case derive of
        Nil -> eqListSigma (append orig p) (append p (concat imag))
        Cons x xs -> 
            and2 (eqListSigma (append orig p) (left_semantics (head derive)))
          ( and2 ( eqListSigma (append p (concat imag)) (right_semantics (last derive)))
          ( and2 (derivation_for_system r derive ) True ) )
                )

rs0 = RS (Cons (Rule (Cons A Nil)
                    (Cons B (Cons A (Cons B Nil))))
          Nil)

-- rewriting system  ab -> bbaa.

rs1 = RS (Cons (Rule (Cons A(Cons B Nil))
                    (Cons B(Cons B (Cons A (Cons A Nil)))) )
          Nil)




e08 = RS 
   (Cons (Rule (Cons A(Cons A(Cons B(Cons B Nil))))
               (Cons B(Cons B(Cons B(Cons A Nil)))))
   (Cons (Rule (Cons B(Cons A Nil))
               (Cons A(Cons A(Cons A(Cons A Nil)))))
         Nil))

-- has loop of length 15, cf.
-- http://termcomp.uibk.ac.at/termcomp/competition/resultDetail.seam?resultId=288357&cid=3093
g03 = RS 
   (Cons (Rule (Cons A(Cons A(Cons A(Cons A Nil))))
               (Cons A(Cons A(Cons B(Cons B Nil)))))
   (Cons (Rule (Cons B(Cons A(Cons A(Cons B Nil))))
               (Cons A(Cons A(Cons B(Cons A Nil)))))
         Nil))

g05 = RS 
   (Cons (Rule (Cons A(Cons A(Cons A(Cons A Nil))))
               (Cons A(Cons B(Cons A(Cons B Nil)))))
   (Cons (Rule (Cons B(Cons A(Cons A(Cons B Nil))))
               (Cons A(Cons A(Cons B(Cons A Nil)))))
         Nil))

-- KnockedForLoops: Loop of length 27 starting with a string of length 15
-- OK, der wird auch gefunden (!) mit:
-- {-# OPTIONS_CO4 SizedList Nat30 (SizedStep Nat11 Nat4 Nat4 Nat11) #-}
g06 = RS 
   (Cons (Rule (Cons A(Cons A(Cons A(Cons A Nil))))
               (Cons A(Cons B(Cons A(Cons B Nil)))))
   (Cons (Rule (Cons B(Cons A(Cons A(Cons B Nil))))
               (Cons A(Cons B(Cons A(Cons A Nil)))))
         Nil))

-- KnockedForLoops: Loop of length 80 starting with a string of length 21
g10 = RS 
   (Cons (Rule (Cons A(Cons A(Cons A(Cons A Nil))))
               (Cons A(Cons B(Cons B(Cons B Nil)))))
   (Cons (Rule (Cons B(Cons A(Cons A(Cons B Nil))))
               (Cons A(Cons A(Cons B(Cons A Nil)))))
         Nil))

-- no looping derivations known:
g13 = RS 
   (Cons (Rule (Cons A(Cons A(Cons A(Cons A Nil))))
               (Cons A(Cons B(Cons B(Cons B Nil)))))
   (Cons (Rule (Cons B(Cons A(Cons B(Cons B Nil))))
               (Cons A(Cons B(Cons B(Cons A Nil)))))
         Nil))
g19 = RS 
   (Cons (Rule (Cons A(Cons A(Cons A(Cons A Nil))))
               (Cons B(Cons A(Cons B(Cons B Nil)))))
   (Cons (Rule (Cons B(Cons A(Cons A(Cons B Nil))))
               (Cons A(Cons A(Cons B(Cons A Nil)))))
         Nil))
g20 = RS 
   (Cons (Rule (Cons A(Cons A(Cons A(Cons A Nil))))
               (Cons B(Cons A(Cons B(Cons B Nil)))))
   (Cons (Rule (Cons B(Cons A(Cons A(Cons B Nil))))
               (Cons A(Cons B(Cons A(Cons A Nil)))))
         Nil))


data Bool = False | True
    -- deriving Prelude.Show

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
    -- deriving Prelude.Show

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
    -- deriving Prelude.Show

null xs = case xs of
    Nil -> True
    Cons x xs -> False

head xs = case xs of
    Nil -> undefined
    Cons x xs -> x

tail xs = case xs of
    Nil -> undefined
    Cons x xs -> xs

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

concat xss = case xss of
    Nil -> Nil
    Cons xs xss -> append xs (concat xss)

append xs ys = case xs of
    Nil -> ys
    Cons x xs -> Cons x (append xs ys)

factor xs ys = or2 (prefix xs ys ) ( case ys of
    Nil -> False
    Cons y ys -> factor xs ys )

prefix xs ys = case xs of
    Nil -> True
    Cons x xs -> case ys of
        Nil -> False
        Cons y ys -> 
            and2 (eqSigma x y) (prefix xs ys)

foldr f z xs = case xs of
    Nil -> z
    Cons x xs -> f x (foldr f z xs)

map f xs = foldr ( \ x y -> Cons (f x) y ) Nil xs

or  xs = foldr or2 False xs
and xs = foldr and2 True xs

forall xs f = and ( map f xs )
exists xs f = or  ( map f xs )

data Rule = Rule (List Sigma) (List Sigma)
    -- deriving Prelude.Show

eqRule u1 u2 = case u1 of
        Rule l1 r1 -> case u2 of
            Rule l2 r2 -> 
                and2 (eqListSigma l1 l2)
                     (eqListSigma r1 r2)

data RS = RS (List Rule)

data Step = Step (List Sigma) -- prefix
                 Rule
                 (List Sigma) -- suffix
    -- deriving Prelude.Show

-- type Derivation = List Step

looping_derivation rs d =
  and2 (break_symmetry d)
   ( and2 (derivation_is_nonempty d)
    ( and2 (derivation_uses_rules rs d)
      (and2 (derivation_is_joinable d)
           (derivation_is_looping d))))

derivation_for_system rs d = 
   and2 (derivation_is_nonempty d)
    ( and2 (derivation_uses_rules rs d)
      (and2 (derivation_is_joinable d) True ) )

break_symmetry d = case d of
    Nil -> False
    Cons s ss -> case s of
        Step pre r suf -> null pre

derivation_uses_rules rs d = case rs of
    RS rules -> forall d
        ( \ s -> step_uses_rules rules s )

derivation_is_nonempty d = not (null d)

derivation_is_looping d = 
      factor (left_semantics (head d))
           (right_semantics (last d))

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

joinable_steps step1 step2 = 
    eqListSigma (right_semantics step1)
                (left_semantics  step2)

step_uses_rules rules step = case step of
   Step p u s -> 
       exists rules ( \ v -> eqRule u v )

