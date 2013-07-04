module CO4.Test.Loop where

-- * string rewriting:

type Symbol =  [ Bool ]
type Word = [ Symbol ]
type Rule = ( Word , Word )
type SRS = [ Rule ]

-- * top level constraint

data Looping_Derivation =
     Looping_Derivation Word [Step] Word

constraint :: SRS -> Looping_Derivation -> Bool
constraint srs (Looping_Derivation pre d suf) =
  conformant srs d && eqWord (pre ++ start d ++ suf) (result d)

isInfix xs ys = 
    any ( \ zs -> isPrefixOf xs zs) (tails ys)

isPrefixOf xs ys = case xs of
    [] -> True
    x:xs' -> case ys of
        [] -> False
        y:ys' -> (eqSym x y) && isPrefixOf xs' ys'

tails xs = xs : case xs of
    [] -> []
    x : xs' -> tails xs'

-- * derivation

-- | Step p (l,r) s  
-- denotes the step from  p++l++s to p++r++s .
-- Names: p = prefix, s = suffix
data Step = Step Word Rule Word

left_value :: Step -> Word
left_value step = case step of
    Step p u s -> case u of 
        (l,r) -> p ++ (l ++ s)

right_value :: Step -> Word
right_value step = case step of
    Step p u s -> case u of 
        (l,r) -> p ++ (r ++ s)

type Derivation = [Step]

start :: Derivation -> Word
start steps = case steps of
    [] -> undefined
    x : ys -> left_value x

-- | the result of a derivation
-- (and undefined if derivation is inconsistent)
result :: Derivation -> Word
result steps = case steps of
    [] -> undefined
    x : ys -> case ys of 
        [] -> right_value x
        y : zs -> case eqWord (right_value x) (left_value y) of
            False -> undefined
            True  -> result ys

conformant :: SRS -> Derivation -> Bool
conformant srs steps = 
    all ( \(Step p u s) -> elemRule u srs ) steps

elemRule u srs = 
    any ( \ v -> eqRule u v ) srs

-- * argh

eqRule u v = case u of
    (u1,u2) -> case v of
        (v1,v2) -> eqWord u1 v1 && eqWord u2 v2
eqWord = eqList eqSym
eqSym = eqList eqBool

eqBool x y = case x of
    False -> not y
    True -> y    

eqList comp xs ys = case xs of
    [] -> case ys of []  -> True ;  _ -> False
    x : xs' -> case ys of
        [] -> False
        y : ys' -> comp x y && eqList comp xs' ys'

