{-# OPTIONS_CO4 SizedInter Nat3 Nat3 Nat3 Nat3 Nat3 Nat3 #-}

-- import qualified Prelude 
-- undefined = Prelude.undefined

-- Booleans

data Bool   = False | True

or2 x y = case x of
   False -> y
   True -> True

and2 x y = case x of
   False -> False
   True -> y

-- Lists, matrices

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

-- map f xs = foldr ( \ x y -> Cons (f x) y) xs
map f xs = case xs of
    Nil -> Nil
    Cons x xs' -> Cons (f x) (map f xs')

flip f x y = f y x

for xs f = flip map f xs

transpose l = case l of
    Nil -> Nil
    Cons xs xss -> case xs of
        Nil -> transpose xss
        Cons x xs' -> 
           Cons (Cons x (map head xss))
                (transpose (Cons xs' (map tail xss)))

dot xs ys = case xs of
    Nil -> I
    Cons x xs' -> case ys of
        Nil -> I
        Cons y ys' -> fplus (ftimes x y) (dot xs' ys')

mtimes a b = 
   let t = transpose b 
   in 
   -- for a ( \ row -> for t ( \ col -> dot row col)) 
     map (\ row -> map (\col -> dot row col) t) a

-- strict zip with bool
szwb f xs ys = case xs of
    Nil -> case ys of
        Nil -> True
        Cons y ys' -> False
    Cons x xs' -> case ys of
        Nil -> False
        Cons y ys' -> and2 (f x y) (szwb f xs' ys')

vector_gt xs ys = szwb fgt xs ys
matrix_gt a b = szwb vector_gt a b

mshaped a = case a of
    Nil -> False
    Cons xs xss -> case xs of
        Nil -> False
        Cons x xs' -> case x of
            I -> False
            F u -> True

-- interpretation for boolean alphabet
data Inter = Inter (List (List F)) (List (List F))

ishaped i = case i of
   Inter a b -> and2 (mshaped a) (mshaped b)

value i xs = case i of 
    Inter a b -> case xs of
        Nil -> undefined
        Cons x xs' -> case xs' of
            Nil -> case x of
                False -> a
                True  -> b
            Cons y ys -> mtimes (case x of
                False -> a
                True -> b) (value i xs')

compatible i lhs rhs = 
    matrix_gt (value i lhs) (value i rhs)

-- unary numbers

data U = Z | S U 

u2 zz zs sz ss x y = 
    case x of  
        Z -> case y of
            Z -> zz
            S y' -> zs y'
        S x' -> case y of
            Z -> sz x'
            S y' -> ss ( u2 zz zs sz ss x' y' ) 

const x y = x
id x = x

eq = u2 True ( const False )(const False) id
min = u2 Z ( const Z ) (const Z) S
max = u2 Z id id S
gt = u2 False ( const False ) (const True) id

-- fuzzy numbers

data F = I | F U

fgt x y = case x of
    I -> True
    F u -> case y of
        I -> False
        F v -> gt u v

fplus x y = case x of
    I -> y
    F u -> case y of
        I -> x
        F v -> F (min u v)

ftimes x y = case x of
    I -> I
    F u -> case y of
        I -> I
        F v -> F (max u v)

-- main program:
-- find a fuzzy matrix interpretation
-- that is compatible with  a a -> a b a
-- (where a = False, b = True)
main i = 
    let lhs = Cons False (Cons False Nil)
        rhs = Cons False (Cons True (Cons False Nil))
    in  and2 
            (ishaped i) 
            ( compatible i lhs rhs)
