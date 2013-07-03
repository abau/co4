{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

import Test.SmallCheck
import Test.SmallCheck.Series

class Semiring a where
    zero :: a ; one :: a
    plus :: a -> a -> a ; times :: a -> a -> a
    gt :: a -> a -> Bool
    ge :: a -> a -> Bool

associative f = \ x y z -> 
    f (f x y) z == f x (f y z)
commutative f = \ x y -> 
    f x y == f y x
distributive_left f g = \ x y z -> 
    f (g x y) z == g (f x z) (f y z)
distributive_right f g = \ x y z -> 
    f x (g y z) == g (f x y) (f x z)

strict f = \ a b c -> 
    ((c /= zero) && gt a b ) <= gt (f a c)(f b c)
half_strict f = \ a b c d -> 
    (gt a b && gt c d) <= gt (f a c) (f b d)

data A = M | F Integer 
    deriving (Show, Ord, Eq)

instance Monad m => Serial m A where
    series = cons0 M \/ cons1 F

instance Semiring A where
    zero = M ; one = F 0
    plus x y = case x of 
        M -> y ; F i -> case y of
            M -> x
            F j -> F (max i j)
    times x y = case x of
        M -> x ; F i -> case y of
            M -> y
            F j -> F (i + j)
    gt x y = x > y ; ge x y = x >= y

data Q = Q A A
    deriving (Show, Ord, Eq)

instance Monad m => Serial m Q where
    series = cons2 ( \ x y -> Q (max x y) (min x y))

instance Semiring Q where
    zero = Q zero zero ; one = Q one zero
    plus (Q x1 y1) (Q x2 y2) = 
        Q (plus x1 x2)
          (plus (min x2 x1) (plus y1 y2))
    times (Q x1 y1) (Q x2 y2) = 
        Q (times x1 x2)
          (plus (times x1 y2) (times x2 y1))
    gt x y = x > y
    ge x y = x >= y

