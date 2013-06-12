module CO4.Test.Loop where

-- * string rewriting:

type Symbol =  [ Bool ]
type Word = [ Symbol ]
type Rule = ( Word , Word )
type SRS = [ Rule ]

-- * top level constraint

main :: SRS -> Derivation -> Bool
main srs d = 
        conformant srs d
    &&  isInfix (start d) (result d)

isInfix xs ys = 
    any ( \ zs -> isPrefixOf xs zs) (tails ys)

isPrefixOf xs ys = case xs of
    [] -> True
    x:xs' -> case ys of
        [] -> False
        y:ys' -> (x == y) && isPrefixOf xs' ys'

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
        y : zs -> case right_value x == left_value y of
            False -> undefined
            True  -> result ys

conformant :: SRS -> Derivation -> Bool
conformant srs steps = 
    all ( \ step -> case step of
          Step p u s -> elemRule u srs ) steps

elemRule u srs = 
    any ( \ v -> u == v ) srs


