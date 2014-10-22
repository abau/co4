{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           System.Environment (getArgs)
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Test.RFCSymmetricBitfieldStandalone

$( compileFile [Cache,ImportPrelude] "test/CO4/Test/RFCSymmetricBitfieldStandalone.hs" )

allocator :: Int -> TAllocator [Grid]
allocator n = allocatorList $ map allocGrid [q1,q2,q3,q4]
  where
    bits          = map (\i -> allocatorId i complete) [1..(n*n)]
    rotate        = {-map (map (\x -> succ x `mod` (n*n))) . -} map reverse . transpose

    [q1,q2,q3,q4] = take 4
                  $ iterate rotate
                  $ take n $ iterate (map (+n)) [0..n-1]

    allocGrid     = allocatorList . map (allocatorList . map (bits !!))

allocator2 :: Int -> TAllocator Grid
allocator2 r = kList r $ kList r complete

toIndex :: Int -> Index
toIndex 1 = This
toIndex n = Next $ toIndex $ n - 1

result :: Int -> IO String
result rs = do
  grids <- solveAndTestP (toIndex rs, toIndex rs) (allocator rs) encConstraint constraint
  case grids of
    Nothing -> return "Nothing"
    Just gs -> return $ unlines $ map showGrid gs

showGrid :: Grid -> String
showGrid = unlines . map (map toChar)
  where
    toChar False = '.'
    toChar True  = '*'

main :: IO ()
main = do
  [rs] <- getArgs >>= return . map read
  result rs >>= putStrLn
