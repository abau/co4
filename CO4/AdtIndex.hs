{-# LANGUAGE ExistentialQuantification #-}
module CO4.AdtIndex
  ( Index (..), AdtIndex (..), ConstructorIndex (..), ArgumentIndex (..)
  , Indexed (..), IndexedWrapper (..)
  , numConstructors, adtIndex, staticAdtWidth
  , constructorArgument, constructorArguments
  , allConstructorArguments, atIndex, staticAdtIndex, staticOffset, staticNormalize 
  {-, merge-})
where

import           Control.Exception (assert)
import           Data.Tree
import           CO4.Util (bitWidth)

data Index = Index { from :: Int, width :: Int } deriving (Eq,Show)

data AdtIndex = AdtIndex { flagIndex    :: Index
                         , constructors :: [ConstructorIndex] 
                         } deriving (Eq)

data ConstructorIndex = ConsNormal [ArgumentIndex]
                      | ConsBottom 
                      deriving (Eq)

data ArgumentIndex = ArgIndex { isStatic :: Bool
                              , argIndex :: AdtIndex
                              }
                              deriving (Eq)

class Indexed a where
  index       :: Int -> a -> AdtIndex
  isRecursive :: a -> Bool

data IndexedWrapper = forall a . Indexed a => IndexedWrapper a

instance Show AdtIndex where
  show = drawTree . toTree

numConstructors :: AdtIndex -> Int
numConstructors = length . constructors

adtIndex :: Int -> [Maybe [IndexedWrapper]] -> AdtIndex
adtIndex startFrom constructorData = AdtIndex flagIndex constructors
  where
    flagWidth    = bitWidth $ length constructorData
    flagIndex    = Index startFrom flagWidth
    constructors = map (toConstructorIndex $ startFrom + flagWidth) constructorData
    
    toConstructorIndex _ Nothing   = ConsBottom
    toConstructorIndex i (Just as) = ConsNormal $ snd $ foldl toArgumentIndex (i,[]) as

    toArgumentIndex (i,args) (IndexedWrapper a) =
      if isRecursive a
      then (i, args ++ [ ArgIndex False $ index 0 a ])
      else
        let a'      = index i a
            a'Width = staticAdtWidth a'
        in
          (i + a'Width, args ++ [ ArgIndex True a' ])
            
staticAdtWidth :: AdtIndex -> Int
staticAdtWidth adt = (width $ flagIndex adt) + ( maximum 
                                               $ map staticConstructorWidth 
                                               $ constructors adt)
  where
    staticConstructorWidth (ConsNormal args) = sum $ map staticArgWidth args
    staticConstructorWidth ConsBottom        = 0

    staticArgWidth arg | isStatic arg = staticAdtWidth $ argIndex arg
    staticArgWidth _                  = 0

constructorArgument :: Int -> Int -> AdtIndex -> Maybe ArgumentIndex
constructorArgument i j adt = case constructorArguments j adt of
  Nothing   -> Nothing
  Just args -> assert (i < length args) $ Just $ args !! i

constructorArguments :: Int -> AdtIndex -> Maybe [ArgumentIndex]
constructorArguments j adt = assert (j < length (constructors adt)) 
                           $ allConstructorArguments adt !! j  

allConstructorArguments :: AdtIndex -> [Maybe [ArgumentIndex]]
allConstructorArguments = map go . constructors
  where 
    go (ConsNormal args) = Just args
    go ConsBottom        = Nothing

atIndex :: [a] -> Index -> [a]
atIndex   xs   (Index 0 n) = take n xs
atIndex (_:xs) (Index i n) = atIndex xs $ Index (i-1) n

staticAdtIndex :: AdtIndex -> Index
staticAdtIndex adt = Index (from $ flagIndex adt) (staticAdtWidth adt)

staticOffset :: Int -> AdtIndex -> AdtIndex
staticOffset n adt = AdtIndex (offsetIndex $ flagIndex adt)
                              $ map offsetConstructor $ constructors adt
  where
    offsetIndex i                       = i { from = from i + n }
    offsetConstructor ConsBottom        = ConsBottom
    offsetConstructor (ConsNormal adts) = ConsNormal $ map offsetArg adts
    offsetArg a   | isStatic a          = a { argIndex = staticOffset n $ argIndex a }
    offsetArg a                         = a 

staticNormalize :: AdtIndex -> AdtIndex
staticNormalize adt = staticOffset (negate $ from $ flagIndex adt) adt

{-
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
      -}

toTree :: AdtIndex -> Tree String
toTree (AdtIndex ix cons) = Node (show ix) $ map consToTree $ zip [0..] cons
  where
    consToTree (i,ConsBottom)      = Node ("cons " ++ show i ++ ": _|_") []
    consToTree (i,ConsNormal args) = Node ("cons " ++ show i) $ map argToTree args

    argToTree (ArgIndex True  (AdtIndex ix cons)) = 
      Node ("static " ++ show ix)    $ map consToTree $ zip [0..] cons

    argToTree (ArgIndex False (AdtIndex ix cons)) = 
      Node ("recursive " ++ show ix) $ map consToTree $ zip [0..] cons
