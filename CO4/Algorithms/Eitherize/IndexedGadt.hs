{-# LANGUAGE ExistentialQuantification #-}
module CO4.Algorithms.Eitherize.IndexedGadt
  ( IndexedGadt (flagIndex), Indexed (..), Index (..), IndexedWrapper (..)
  , numConstructors, indexedGadt, gadtWidth, constructorArgument
  , constructorArguments, allConstructorArguments, undefinedGadtPaths
  , atIndex, indexOfGadt, offset, normalize, merge)
where

import           Control.Exception (assert)
import           Data.Tree
import           CO4.Util (bitWidth)

data IndexedGadt = IndexedGadt { flagIndex    :: Index
                               , constructors :: [IndexedConstructor] 
                               } deriving (Eq)

data IndexedConstructor = ConsNormal [IndexedGadt]
                        | ConsUndefined 
                        deriving (Eq)

data Index = Index { from :: Int, width :: Int } deriving (Eq,Show)

class Indexed a where
  index :: Int -> a -> IndexedGadt

data IndexedWrapper = forall a . Indexed a => IndexedWrapper a

instance Indexed IndexedGadt where
  index = offset

instance Show IndexedGadt where
  show = drawTree . toTree

numConstructors :: IndexedGadt -> Int
numConstructors = length . constructors

indexedGadt :: Int -> [Maybe [IndexedWrapper]] -> IndexedGadt
indexedGadt startFrom constructorData = IndexedGadt flagIndex constructors
  where
    flagWidth    = bitWidth $ length constructorData
    flagIndex    = Index startFrom flagWidth
    constructors = map (toIndexedConstructor $ startFrom + flagWidth) constructorData
    
    toIndexedConstructor _ Nothing   = ConsUndefined
    toIndexedConstructor i (Just as) = ConsNormal $ snd $ 
      foldl (\(i',as') (IndexedWrapper a) -> 
                let a'      = index i' a
                    a'Width = gadtWidth a'
                in
                  (i' + a'Width, as' ++ [a'])
      ) (i,[]) as
            
gadtWidth :: IndexedGadt -> Int
gadtWidth gadt = flagWidth + (maximum $ map constructorWidth $ constructors gadt)
  where
    flagWidth = width $ flagIndex gadt

constructorWidth :: IndexedConstructor -> Int
constructorWidth (ConsNormal gadts) = foldl (\w arg -> w + gadtWidth arg) 0 gadts
constructorWidth ConsUndefined      = 0

constructorArgument :: Int -> Int -> IndexedGadt -> Maybe IndexedGadt
constructorArgument i j gadt = case constructorArguments j gadt of
  Nothing   -> Nothing
  Just args -> assert (i < length args) $ Just $ args !! i

constructorArguments :: Int -> IndexedGadt -> Maybe [IndexedGadt]
constructorArguments j gadt = assert (j < length (constructors gadt)) 
                            $ allConstructorArguments gadt !! j  

allConstructorArguments :: IndexedGadt -> [Maybe [IndexedGadt]]
allConstructorArguments = map go . constructors
  where 
    go (ConsNormal args) = Just args
    go ConsUndefined     = Nothing

undefinedGadtPaths :: IndexedGadt -> [[(Index,Int)]]
undefinedGadtPaths = go []
  where
    go path gadt = concatMap fromConstructor $ zip [0..] $ constructors gadt
      where
        fromConstructor (consIx, ConsNormal args) = 
          concatMap (go (path' consIx)) args

        fromConstructor (consIx, ConsUndefined)   = [path' consIx]
        path' ix                                  = path ++ [(flagIndex gadt, ix)]

atIndex :: [a] -> Index -> [a]
atIndex   xs   (Index 0 n) = take n xs
atIndex (_:xs) (Index i n) = atIndex xs $ Index (i-1) n

indexOfGadt :: IndexedGadt -> Index
indexOfGadt gadt = Index (from $ flagIndex gadt) (gadtWidth gadt)

offset :: Int -> IndexedGadt -> IndexedGadt
offset n gadt = IndexedGadt (offsetIndex $ flagIndex gadt)
                          $ map offsetConstructor $ constructors gadt
  where
    offsetIndex i                        = i { from = from i + n }
    offsetConstructor ConsUndefined      = ConsUndefined
    offsetConstructor (ConsNormal gadts) = ConsNormal $ map (offset n) gadts

normalize :: IndexedGadt -> IndexedGadt
normalize gadt = offset (negate $ from $ flagIndex gadt) gadt

merge :: IndexedGadt -> IndexedGadt -> IndexedGadt
merge a b = IndexedGadt (mergeIndex (flagIndex a) (flagIndex b))
                        (mergeConstructors (constructors a) (constructors b))
  where 
    mergeIndex x y        = assert (x == y) x
    mergeConstructors x y = assert (length x == length y)
                            $ zipWith mergeConstructor x y

    mergeConstructor x ConsUndefined               = x
    mergeConstructor ConsUndefined y               = y
    mergeConstructor (ConsNormal x) (ConsNormal y) = 
      assert (length x == length y) $ ConsNormal $ zipWith merge x y

toTree :: IndexedGadt -> Tree String
toTree (IndexedGadt ix cons) = Node (show ix) $ map toTree' $ zip [0..] cons
  where
    toTree' (i,ConsUndefined)    = Node (show i) [Node "undefined" []]
    toTree' (i,ConsNormal gadts) = Node (show i) $ map toTree gadts
