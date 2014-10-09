module CO4.Test.RFCStandalone
where

import CO4.Prelude

type Grid    = [[Color]]
type Color   = Nat
data Index   = This | Next Index deriving Show
type Index2D = (Index,Index)
type Rect    = (Index2D,Index2D)

testGrid :: Grid
testGrid = [ [nat 2 0, nat 2 0]
           , [nat 2 0, nat 2 0]
           ]
test = constraint (Next This, Next This) testGrid

constraint :: (Index,Index) -> Grid -> Bool
constraint dimension grid = 
  let rects = rectangles dimension
  in
    all (\r -> not (isMonochromatic r grid)) rects

isMonochromatic :: Rect -> Grid -> Bool
isMonochromatic (lo,hi) grid = case lo of 
  (loR,loC) -> case hi of 
    (hiR,hiC) -> let c1 = colorAt grid (loR,loC)
                     c2 = colorAt grid (loR,hiC)
                     c3 = colorAt grid (hiR,loC)
                     c4 = colorAt grid (hiR,hiC)
                     in
                       (eqColor c1 c2) && (eqColor c2 c3) && (eqColor c3 c4)

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

eqColor :: Color -> Color -> Bool
eqColor a b = eqNat a b

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
