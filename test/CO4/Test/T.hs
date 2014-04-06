{-# OPTIONS_CO4 SizedList Nat3 (SizedBool) #-}

main x = or x

data Bool = False | True

and2 x y = case x of
   False -> False
   True -> y

not x = case x of
   False -> True
   True -> False


or2 x y = case x of
   False -> y
   True -> True
{-
or2 False y = y
or2 True y = True
-}

data Pair = Pair Bool Bool

data List a = Nil | Cons a (List a)

head l = case l of
    Nil -> undefined
    Cons x xs -> x

tail l = case l of
    Nil -> undefined
    Cons x xs -> xs

foldr f z xs = case xs of
    Nil -> z
    Cons x xs' -> f x (foldr f z xs')

and = foldr and2 True
or  = foldr or2 False





