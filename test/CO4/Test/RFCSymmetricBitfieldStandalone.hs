module CO4.Test.RFCSymmetricBitfieldStandalone
where

import CO4.Prelude
import Data.List (tails)

type Grid    = [[Color]]
type Color   = Bool
data Index   = This | Next Index deriving Show
type Index2D = (Index,Index)
type Rect    = (Index2D,Index2D)

constraint :: [Grid] -> Bool
constraint grids = (noMonochromaticRect (head grids)) && (reduceGrids or grids)

reduceGrids :: ([Color] -> Bool) -> [Grid] -> Bool
reduceGrids f grids = 
  let reduceRows rs = all f (transpose rs)
  in
    all reduceRows (transpose grids)

noMonochromaticRect :: Grid -> Bool
noMonochromaticRect grid = 
  all (\(row1,row2) ->
      all (\(x,y) -> not (and [fst x, snd x, fst y, snd y])) (pairs (zip row1 row2))
  ) (pairs grid)

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
