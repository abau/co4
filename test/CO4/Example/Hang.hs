-- | encoding for the "1:n" picture hanging problem
-- Section 3 of http://arxiv.org/abs/1203.3602

{-# language TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import CO4
import CO4.Prelude
import Satchmo.Core.Decode
import Control.Applicative
import System.Environment

import CO4.Example.HangStandalone

$( compileFile [ ImportPrelude
               -- , InstantiationDepth 20
               -- , Profile
               , Cache
               ] 
  "test/CO4/Example/HangStandalone.hs" )

-- that's what I want to write:
-- uTurn b = Turn <$> complete <*> uNat b

uTurn :: Int -> TAllocator Turn
uTurn b = unsafeTAllocator
        $ constructors [ Just [ toAllocator uBool
                              , toAllocator $ uNat b]]

uBool :: TAllocator Bool
uBool = complete

uPair :: TAllocator a -> TAllocator b -> TAllocator (a,b)
uPair a b = unsafeTAllocator
    $ constructors [ Just [ toAllocator a, toAllocator b ] ]

-- uStack :: Int -> Int -> TAllocator Stack
-- uStack d b = uList d $ uTurn b

main = do
  putStrLn $ unlines
    [ "1:n picture hanging problem"
    , "inputs: no. of pins, length of word"
    ]
  [ s, n ] <- getArgs
  result (read s) (read n) 

-- | successful example calls:
--  result 2 4, result 3 10, result 4 16, result 5 28.
-- The challenge is to find a solution for @s@
-- that is shorter  @bestknown s@ (see below)
-- or prove that it's impossible.

result s n = do
  let b = ceiling $ logBase 2 $ fromIntegral s
  out <- -- solveAndTestP
    solveP (nat b $ s - 1)
      (uPair ( kList n $ uTurn b )
          (allocatorList $ map (\p -> fromKnown $ nat b p) [0..s-1])
       )
      encConstraint
      -- constraint
  print $ out

bestknown s =
  if s == 1 then 1
  else let h = div s 2 in 2 * ( bestknown h + bestknown (s - h))
  
