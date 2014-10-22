module CO4.Test.RFCSymmetricBitfieldStandalone
where

import CO4.Prelude

type Grid    = [[Color]]
type Color   = Bool
data Index   = This | Next Index deriving Show
type Index2D = (Index,Index)
type Rect    = (Index2D,Index2D)

constraint :: (Index,Index) -> [Grid] -> Bool
constraint dimension grids = 
  let rects          = rectangles dimension
      validBitfields = validBitfield rects (head grids)
      oneColor       = reduceGrids or grids
  in
    validBitfields && oneColor

validBitfield :: [Rect] -> Grid -> Bool
validBitfield rects grid = all (\r -> not (isMonochromatic r grid)) rects

reduceGrids :: ([Color] -> Bool) -> [Grid] -> Bool
reduceGrids f grids = 
  let reduceRows rs = all f (transpose rs)
  in
    all reduceRows (transpose grids)

isMonochromatic :: Rect -> Grid -> Bool
isMonochromatic (lo,hi) grid = case lo of 
  (loR,loC) -> case hi of 
    (hiR,hiC) -> let c1 = colorAt grid (loR,loC)
                     c2 = colorAt grid (loR,hiC)
                     c3 = colorAt grid (hiR,loC)
                     c4 = colorAt grid (hiR,hiC)
                     in
                       and [c1,c2,c3,c4]

rectangles :: (Index,Index) -> [Rect]
rectangles (dimR,dimC) = 
  concatMap (\hiR -> 
    concatMap (\hiC -> 
      concatMap (\loR -> 
        concatMap (\loC -> [((loR,loC),(hiR,hiC))]) 
        (indices This (prev hiC)))
      (indices This (prev hiR)))
    (indices (Next This) dimC)) 
  (indices (Next This) dimR)

indices :: Index -> Index -> [Index]
indices from to = case eqIndex from to of
  True  -> [from]
  False -> from : (indices (Next from) to)

eqIndex :: Index -> Index -> Bool
eqIndex a b = case assertKnown a of
  This -> case assertKnown b of 
    This -> True
    _    -> False
  Next a' -> case assertKnown b of 
    This    -> False
    Next b' -> eqIndex a' b'

colorAt :: Grid -> (Index,Index) -> Color
colorAt grid (r,c) = at (at grid r) c

at :: [a] -> Index -> a
at xs i = case assertKnown i of
  This    -> head xs
  Next i' -> at (tail xs) i'

prev :: Index -> Index
prev i = case assertKnown i of
  This    -> undefined
  Next i' -> i'

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
