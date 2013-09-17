module CO4.CSPlib.Prob006.Constraint where

import Prelude hiding ( sum )
import CO4.Prelude

constraint len diffs = 
       leNat (sum diffs) len
    && alldifferent 
        -- ( map sum ( segments diffs ))
       ( map_sum_segments diffs )


-- naive implementation
alldifferent_naive xs = case xs of
    [] -> True
    x : ys -> all ( \ y -> not (eqNat x y)) ys
          && alldifferent ys

alldifferent xs = 
   neighbour_different ( oesort xs )

neighbour_different xs = case xs of
    [] -> True
    x : xs' -> case xs' of
        [] -> True
        y : ys' -> 
            not (eqNat x y) && neighbour_different xs'

-- FIXME
oesort xs = case xs of
    [] -> []
    x : xs' -> case xs' of
        [] -> [x]
        x' : xs'' -> 
            let (ys,zs) = distribute xs
            in  oemerge (oesort ys) (oesort zs)

-- | distribute [0..4] = ([0,2,4],[1,3])
distribute xs = case xs of
    [] -> ( [], [] )
    x : xs' -> let (ys,zs) = distribute xs'
               in  (x : zs, ys)


oemerge xs ys = case xs of
    [] -> fuse xs ys
    x : xs' -> case xs' of
      [] -> fuse xs ys
      x' : xs'' -> 
        case ys of
            [] -> fuse xs ys
            y : ys' -> case ys' of
                [] -> fuse xs ys
                y' : ys'' -> 
                  let (xes, xos) = distribute xs
                      (yes, yos) = distribute ys
                  in  repair (oemerge xes yes)
                           (oemerge xos yos) 

fuse = repair

repair xs ys = case xs of
    [] -> ys 
    x : xs' -> case ys of
        [] -> xs
        y : ys' -> case ( case ltNat x y of
                      True -> (x,y) ; False -> (y,x)
                 ) of 
                ( lo, hi ) -> lo : hi : repair xs' ys'

    
-- equivalent to map sum $ segments xs
-- but uses fewer additions (?)
map_sum_segments xs = case xs of
    [] -> []
    x : xs' -> map_sum_inits xs ++ map_sum_segments xs'

map_sum_inits xs = case xs of
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
