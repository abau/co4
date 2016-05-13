module CO4.Example.SudokuStandalone where

import CO4.Prelude

data Unary = Z | S Unary deriving Show

type Matrix a = [[a]]
type Block    = Matrix Unary
type Board    = Matrix Block

constraint :: Board -> Bool
constraint board = 
  let n = unaryLength board
      n2 = unaryLength $ concat board
      allDifferent xs = allOccur (unaryBelow n2) xs
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

allOccur :: [Unary] -> [Unary] -> Bool
allOccur ns xs = all (\n -> exactlyOne (map (unaryEq n) xs)) ns

exactlyOne :: [Bool] -> Bool
exactlyOne xs = case xs of
  [] -> False
  x:xs' -> case xs' of
    [] -> x
    x':xs'' -> case split xs of
      (ys,zs) -> (exactlyOne ys && none zs) || (exactlyOne zs && none ys)

split :: [a] -> ([a], [a])
split xs = case xs of
  [] -> ([],[])
  x:xs' -> case split xs' of
    (ys,zs) -> (x:zs,ys)

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

unaryBelow :: Unary -> [Unary]
unaryBelow x = case x of
  Z -> []
  S x' -> x' : unaryBelow x'

none :: [Bool] -> Bool
none xs = not $ or xs

transpose :: Matrix a -> Matrix a
transpose xs = case xs of
  [] -> []
  row : rows -> case rows of
    [] -> map ( \ x -> [x] ) row
    _ -> zipWith (:) row (transpose rows)

