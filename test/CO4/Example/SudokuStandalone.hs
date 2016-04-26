module CO4.Example.SudokuStandalone where

import CO4.Prelude

data Unary = Z | S Unary deriving Show

type Matrix a = [[a]]
type Block    = Matrix Nat
type Board    = Matrix Block

constraint :: Nat -> Board -> Bool
constraint nSqr board = 
  let n = unaryLength board
  in
    and [ all (all (\b -> allDifferent $ concat b)) board
        , all allDifferent $ rows board n
        , all allDifferent $ columns board n 
        , all (gtNat nSqr) $ concat $ concat $ concat board
        ]

rows :: Board -> Unary -> [[Nat]]
rows board n = 
  let join = foldl (zipWith (++)) (unaryReplicate n [])
  in
    concatMap join board

columns :: Board -> Unary -> [[Nat]]
columns board n = 
  let transposedBoard = transpose $ map (map transpose) board
  in
    rows transposedBoard n

allDifferent :: [Nat] -> Bool
allDifferent xs = case xs of
  [] -> True
  y:ys -> (allDifferent ys) && (not $ contains y ys)

contains :: Nat -> [Nat] -> Bool
contains x xs = case xs of
  [] -> False
  y:ys -> (eqNat x y) || (contains x ys)

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

transpose :: Matrix a -> Matrix a
transpose xs = case xs of
  [] -> []
  row : rows -> case rows of
    [] -> map ( \ x -> [x] ) row
    _ -> zipWith (:) row (transpose rows)

