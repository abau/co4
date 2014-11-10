module CO4.Test.RFCSymmetricBitfieldStandalone
where

import CO4.Prelude
import Data.List (tails)

type Grid  = [[Color]]
type Color = Bool

constraint :: [Grid] -> Bool
constraint grids = (noMonochromaticRect (head grids)) && (reduceGrids or grids)

noMonochromaticRect :: Grid -> Bool
noMonochromaticRect grid = 
  --all (\(row1, row2) -> atmost1 (zipWith (&&) row1 row2)) (pairs grid)
  all (\(row1, row2) -> atmost (S Z) (zipWith (&&) row1 row2)) (pairs grid)

reduceGrids :: ([Color] -> Bool) -> [Grid] -> Bool
reduceGrids f grids = 
  let reduceRows rs = all f (transpose rs)
  in
    all reduceRows (transpose grids)

pairs :: [a] -> [(a,a)]
pairs list = concatMap (\xs -> case xs of { [] -> []; (x:ys) -> concatMap (\y -> [(x,y)]) ys } ) 
                       (tails list)

transpose :: [[a]] -> [[a]]
transpose list = case assertKnown list of
  [] -> []
  xxs:xss -> 
    case assertKnown xxs of 
      []   -> transpose xss
      x:xs -> let hhead ys = case assertKnown ys of []  -> []
                                                    y:_ -> [y]
                  ttail ys = case assertKnown ys of []  -> []
                                                    _:y -> y
              in
                (x : (concatMap hhead xss)) : transpose (xs : (map ttail xss))

atmost1 :: [Bool] -> Bool
atmost1 xs = case xs of
  []   -> True
  y:ys -> case y of
            False -> atmost1 ys
            True  -> not (or ys)

data N = Z | S N

atmost :: N -> [Bool] -> Bool
atmost n xs = case n of
  Z    -> not (or xs)
  S n' -> case xs of
            []   -> True
            y:ys -> atmost n' ys || (not y && atmost n ys) 
