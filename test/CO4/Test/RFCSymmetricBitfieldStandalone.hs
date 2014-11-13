module CO4.Test.RFCSymmetricBitfieldStandalone
where

import CO4.Prelude
import Data.List (tails)

type Grid  = [[Color]]
type Color = Bool

constraint :: [Grid] -> Bool
constraint = constraint2

constraint1 :: [Grid] -> Bool
constraint1 grids = 
  let noMonochromaticRect g = 
        all (\(row1, row2) -> atmost1 (zipWith (&&) row1 row2)) (pairs g)
  in
    (noMonochromaticRect (head grids)) && (reduceGrids or grids)

constraint2 :: [Grid] -> Bool
constraint2 grids = 
  let noMonochromaticRect g = 
        all (\(row1, row2) -> atmost (S Z) (zipWith (&&) row1 row2)) (pairs g)
  in
    (noMonochromaticRect (head grids)) && (reduceGrids or grids)

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
            True  -> all not ys

data N = Z | S N

atmost :: N -> [Bool] -> Bool
atmost n xs = case xs of
  []   -> True
  y:ys -> case n of
            Z    -> all not xs
            --Z    -> not y && atmost n ys
            S n' -> (not y && atmost n ys) || (atmost n' ys)
