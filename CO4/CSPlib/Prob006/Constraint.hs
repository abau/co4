module CO4.CSPlib.Prob006.Constraint where

import Prelude hiding ( sum )
import CO4.Prelude

import Debug.Trace

constraint len diffs = 
       leNat (sum diffs) len
    && alldifferent_sorting 
     -- ( map sum ( segments diffs ))
        ( map_sum_segments diffs )


-- naive implementation
alldifferent_naive xs = case xs of
    [] -> True
    x : ys -> all ( \ y -> not (eqNat x y)) ys
          && alldifferent_naive ys

alldifferent_sorting xs = 
   monotone ( oesort xs )

monotone xs = case assertKnown xs of
    [] -> True
    x : xs' -> case xs' of
        [] -> True
        y : ys' -> 
            ltNat x y && monotone xs'

oesort xs = case assertKnown xs of
    [] -> xs
    x : xs' -> case assertKnown xs' of
        [] -> xs
        x' : xs'' -> case distribute xs of 
            (ys,zs) -> oemerge (oesort ys) (oesort zs)

-- | distribute [0..4] = ([0,2,4],[1,3])
distribute xs = assertKnown ( case assertKnown xs of
    [] -> ( [], [] )
    x : xs' -> case distribute xs' of
         (ys, zs) -> (x : zs, ys)
                            )

-- applied to monotone sequences where
-- (length xs - length ys) `elem` [0,1]
oemerge xs ys = case assertKnown xs of
    [] -> repair xs ys
    x : xs' -> case assertKnown xs' of
      [] -> repair xs ys
      x' : xs'' -> 
        case assertKnown ys of
            [] -> repair xs ys
            y : ys' ->  case distribute xs of
                  (xos, xes) -> case distribute ys of
                      (yos, yes) ->
                        fuse (oemerge xos yos)
                             (oemerge xes yes) 

fuse xs ys = case assertKnown xs of
    [] -> undefined
    x : xs' -> x : repair xs' ys

repair xs ys = case assertKnown xs of
    [] -> ys 
    x : xs' -> case assertKnown ys of
        [] -> xs
        y : ys' -> 
            minNat x y : maxNat x y : repair xs' ys'
    
-- equivalent to map sum $ segments xs
-- but uses fewer additions (?)
map_sum_segments xs = case assertKnown xs of
    [] -> []
    x : xs' -> map_sum_inits xs ++ map_sum_segments xs'

map_sum_inits xs = case assertKnown xs of
    [] -> []
    x : xs' -> x : map ( \ y -> plusNat x y )
                       (map_sum_inits xs')

-- non-empty contiguous subwords
segments :: [a] -> [[a]]
segments xs = filter (\ ys -> not ( null ys))
     ( concat (map inits (tails xs) ))

inits :: [a] -> [[a]]
inits xs = 
    foldr ( \ x yss -> [] : map ( \ ys -> x : ys) yss)
          [[]] xs

tails :: [a] -> [[a]]
tails xs = xs : case xs of
    [] -> []
    x : xs' -> tails xs'
    
sum xs = case xs of
    [] -> undefined
    x : xs' -> foldr plusNat x xs'
