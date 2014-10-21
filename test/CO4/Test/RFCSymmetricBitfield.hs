{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           System.Environment (getArgs)
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           Data.List (transpose)
import           CO4
import           CO4.Prelude
import           CO4.Test.RFCSymmetricBitfieldStandalone

$( compileFile [Cache,ImportPrelude] "test/CO4/Test/RFCSymmetricBitfieldStandalone.hs" )

allocator :: Int -> TAllocator Grid
allocator n = allocatorList $ map (allocatorList . map (bits !!)) pattern
  where
    nq            = n `div` 2
    bits          = map (\i -> allocatorId i complete) [1..(nq*nq)]
    rotate        = map (map (\x -> succ x `mod` (nq*nq))) . map reverse . transpose

    [q1,q2,q3,q4] = take 4
                  $ iterate rotate
                  $ take nq $ iterate (map (+nq)) [0..nq-1]

    pattern       =  ( map (uncurry (++)) $ zip q1 q2 )
                  ++ ( map (uncurry (++)) $ zip q4 q3 )

allocator2 :: Int -> TAllocator Grid
allocator2 r = kList r $ kList r complete

toIndex :: Int -> Index
toIndex 1 = This
toIndex n = Next $ toIndex $ n - 1

result :: Int -> IO String
result rs = do
  grid <- solveAndTestP (toIndex rs, toIndex rs) (allocator rs) encConstraint constraint
  case grid of
    Nothing -> return "Nothing"
    Just g  -> return $ showGrid g

showGrid :: Grid -> String
showGrid = unlines . map (map toChar)
  where
    toChar False = '0'
    toChar True  = 'X'

main :: IO ()
main = do
  [rs] <- getArgs >>= return . map read
  result rs >>= putStrLn
