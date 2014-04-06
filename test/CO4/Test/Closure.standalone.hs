module CO4.Test.Closure where
import qualified Prelude ; undefined = Prelude.undefined

main d = looping_derivation g03 d

-- rewriting system  ab -> bbaa.

rs1 = RS (Cons (Rule (Cons A(Cons B Nil))
                    (Cons B(Cons B (Cons A (Cons A Nil)))) )
          Nil)


-- Geser's system R_2 (slide 24)
r2 = RS (Cons (Rule (Cons B (Cons A (Cons A Nil)))
                    (Cons A(Cons A (Cons B(Cons B(Cons A Nil)))))) Nil)

-- HofWald1 (slide 22)
hw1 = RS 
    (Cons (Rule (Cons C(Cons B Nil)) (Cons B(Cons B(Cons A Nil))))
    (Cons (Rule (Cons A(Cons B Nil)) (Cons B(Cons C(Cons A Nil))))
          Nil))

uni2 = RS
    (Cons (Rule (Cons A(Cons A(Cons A(Cons A Nil))))
                (Cons A(Cons C(Cons A(Cons C(Cons C Nil))))))
    (Cons (Rule (Cons C(Cons C(Cons C Nil)))
                (Cons A(Cons A(Cons A Nil))))
          Nil))

s12a2n7 = RS
    (Cons (Rule (Cons A(Cons A (Cons A Nil))) 
                Nil)
    (Cons (Rule (Cons B (Cons A(Cons B (Cons B Nil))))
                (Cons A(Cons B(Cons B(Cons B(Cons A Nil))))))
          Nil))

e08 = RS 
   (Cons (Rule (Cons A(Cons A(Cons B(Cons B Nil))))
               (Cons B(Cons B(Cons B(Cons A Nil)))))
   (Cons (Rule (Cons B(Cons A Nil))
               (Cons A(Cons A(Cons A(Cons A Nil)))))
         Nil))

-- Loop of length 15 starting with a string of length 12
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
g06 = RS 
   (Cons (Rule (Cons A(Cons A(Cons A(Cons A Nil))))
               (Cons A(Cons B(Cons A(Cons B Nil)))))
   (Cons (Rule (Cons B(Cons A(Cons A(Cons B Nil))))
               (Cons A(Cons B(Cons A(Cons A Nil)))))
         Nil))

g08 = RS 
   (Cons (Rule (Cons A(Cons A(Cons A(Cons A Nil))))
               (Cons A(Cons B(Cons B(Cons A Nil)))))
   (Cons (Rule (Cons B(Cons A(Cons A(Cons B Nil))))
               (Cons A(Cons A(Cons B(Cons A Nil)))))
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
--        C -> False
    B -> case y of
        A -> False
        B -> True
{-
        C -> False
    C -> case y of
        A -> False
        B -> False
        C -> True
-}

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

eqRule u1 u2 = case u1 of
        Rule l1 r1 -> case u2 of
            Rule l2 r2 -> 
                and2 (eqListSigma l1 l2)
                     (eqListSigma r1 r2)

data RS = RS (List Rule)


-- | a closure is a non-empty derivation
-- data Closure = Closure (List Sigma) (List Sigma)
-- but re re-use data Rule here

data Side = Left | Right | Inside | Outside

data Overlap = Overlap Side (List Sigma) (List Sigma) Rule Rule

overlap_ok c o = case o of
    Overlap side pre suf c1 c2 -> case c of
        Rule l r -> case c1 of
            Rule l1 r1 -> case c2 of
                Rule l2 r2 -> case side of
                    Left -> and2 (eqListSigma l (append pre l1))
                       ( and2 (eqListSigma (append pre r1) (append l2 suf))
                            (eqListSigma (append r2 suf) r) )
                    Right -> and2 (eqListSigma l (append l1 suf))
                       ( and2 (eqListSigma (append r1 suf) (append pre l2))
                            (eqListSigma (append pre r2) r) )        
                    Inside -> and2 (eqListSigma l l1)
                       ( and2 (eqListSigma r1 (append pre (append l2 suf)))
                            (eqListSigma (append pre (append r2 suf)) r) )
                    Outside -> and2 (eqListSigma l (append pre (append l1 suf)))
                       ( and2 (eqListSigma (append pre (append r1 suf)) l2)
                            (eqListSigma r2 r ) )
        
data Step = Step Rule Overlap 
-- type Derivation = List Step

derivation_ok rs d = case d of
    Nil -> True
    Cons step ss -> 
       and2 ( case step of 
            Step c o -> or2 (extension_ok c o ss) (base_ok rs c) )
            ( derivation_ok rs ss )
                      
base_ok rs c = case rs of
    RS us -> exists us ( \ u -> eqCls u c )

extension_ok c o ss = case o of
    Overlap side pre suf c1 c2 -> 
        and2 ( exists ss ( \ s -> case s of Step c o -> eqCls c c1 ) )
            ( and2 ( exists ss ( \ s -> case s of Step c o -> eqCls c c2 ) )
                ( overlap_ok c o ) ) 

eqCls c1 c2 = case c1 of
      Rule l1 r1 -> case c2 of
          Rule l2 r2 -> and2 (eqListSigma l1 l2) (eqListSigma r1 r2)

looping_derivation rs d = 
     and2 ( derivation_ok rs d ) 
        ( self_embedding_derivation d )

self_embedding_derivation d =  case d of
              Nil -> False
              Cons step ss -> case step of
                  Step c o -> case c of
                      Rule l r -> factor l r

