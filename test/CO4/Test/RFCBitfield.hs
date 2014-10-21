{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
module CO4.Test.RFC
where
-}

import           System.Environment (getArgs)
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Test.RFCStandaloneBitfield

$( compileFile [Cache,ImportPrelude] "test/CO4/Test/RFCStandaloneBitfield.hs" )

allocator :: Int -> Int -> Int -> TAllocator [Grid]
allocator r c col = kList col $ kList r $ kList c complete

symAllocator :: Int -> TAllocator [Grid]
symAllocator n = allocatorList [ allocatorList $ map (allocatorList . map (bits !!)) pattern ]
  where
    bits          = map (\i -> allocatorId i complete) [1..n*n]
    rotate        = map (map (\x -> succ x `mod` (n*n))) . map reverse . transpose

    [q1,q2,q3,q4] = take 4
                  $ iterate rotate
                  $ take n $ iterate (map (+n)) [0..n-1]

    pattern       =  ( map (uncurry (++)) $ zip q1 q2 )
                  ++ ( map (uncurry (++)) $ zip q4 q3 )

toIndex :: Int -> Index
toIndex 1 = This
toIndex n = Next $ toIndex $ n - 1

result :: Int -> Int -> (TAllocator [Grid]) -> IO String
result rs cs alloc = do
  grids <- solveAndTestP (toIndex rs, toIndex cs) alloc encConstraint constraint
  case grids of
    Nothing -> return "Nothing"
    Just gs -> return $ unlines $ map showGrid gs

showGrid :: Grid -> String
showGrid = unlines . map (map toChar)
  where
    toChar False = '0'
    toChar True  = 'X'

main :: IO ()
main = do
  args   <- getArgs >>= return . map read
  result <- case args of
              [rs,cs,col] -> result rs cs $ allocator rs cs col
              [rs]        -> result rs rs $ symAllocator rs
  putStrLn result
