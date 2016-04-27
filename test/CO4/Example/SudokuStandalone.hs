module CO4.Example.SudokuStandalone where

import CO4.Prelude

data Unary = Z | S Unary deriving Show

type Matrix a = [[a]]
type Block    = Matrix Unary
type Board    = Matrix Block

constraint :: Board -> Bool
constraint board = 
  let n = unaryLength board
  in
    and [ all (all (\b -> allDifferent $ concat b)) board
        , all allDifferent $ rows board n
        , all allDifferent $ columns board n 
        ]

rows :: Board -> Unary -> [[Unary]]
rows board n = 
  let join = foldl (zipWith (++)) (unaryReplicate n [])
  in
    concatMap join board

columns :: Board -> Unary -> [[Unary]]
columns board n = 
  let transposedBoard = transpose $ map (map transpose) board
  in
    rows transposedBoard n

allDifferent :: [Unary] -> Bool
allDifferent xs = case xs of
  [] -> True
  y:ys -> (allDifferent ys) && (not $ contains y ys)

contains :: Unary -> [Unary] -> Bool
contains x xs = case xs of
  [] -> False
  y:ys -> (unaryEq x y) || (contains x ys)

at :: [a] -> Unary -> a
at xs i = case i of
  Z -> head xs
  S j -> at (tail xs) j

unaryReplicate :: Unary -> a -> [a]
unaryReplicate n a = case n of
  Z -> []
  S m -> a : (unaryReplicate m a)

unaryLength :: [a] -> Unary
unaryLength xs = case xs of
  [] -> Z
  y:ys -> S $ unaryLength ys

unaryEq :: Unary -> Unary -> Bool
unaryEq a b = case a of
  Z -> case b of Z -> True
                 _ -> False
  S a' -> case b of Z -> False
                    S b' -> unaryEq a' b'

transpose :: Matrix a -> Matrix a
transpose xs = case xs of
  [] -> []
  row : rows -> case rows of
    [] -> map ( \ x -> [x] ) row
    _ -> zipWith (:) row (transpose rows)

