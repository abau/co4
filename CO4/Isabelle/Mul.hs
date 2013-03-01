{-# OPTIONS_CO4 SizedList Nat5 SizedBool #-}

main x = eqL 
  (Cons True (Cons False (Cons False (Cons True (Cons True Nil)))))
  (rbl_mult x x)

data Bool = False | True
data List a = Nil | Cons a (List a)

head x = case x of
    Nil -> undefined
    Cons x xs -> x

tail x = case x of
    Nil -> undefined
    Cons x xs -> xs

and2 x y = case x of
    False -> False
    True -> y

eqB x y = case x of
    False -> not y
    True -> y

eqL x y = case x of
    Nil -> case y of
        Nil -> True
        Cons y ys -> False
    Cons x xs -> case y of
        Nil -> False
        Cons y ys -> and2 (eqB x y) (eqL xs ys)

not x = case x of
    False -> True
    True -> False

ite f y n = case f of
    False -> n
    True -> y

-- rbl_succ :: [Bool] -> [Bool];
rbl_succ xs = case xs of
    Nil -> Nil
    Cons x xs -> case x of
        False -> Cons True xs
        True -> Cons False (rbl_succ xs)

-- rbl_add :: [Bool] -> [Bool] -> [Bool];
rbl_add y x = case y of
    Nil -> Nil
    Cons y ys -> 
        let  ws = rbl_add ys (tail x)
        in   Cons (not (eqB y (head x)))
              (ite (and2 (head x) y) (rbl_succ ws) ws)

-- rbl_mult :: [Bool] -> [Bool] -> [Bool];
rbl_mult y x = case y of
    Nil -> Nil
    Cons y ys -> 
      let ws = Cons False (rbl_mult ys x)
      in  (ite y (rbl_add ws x) ws)

