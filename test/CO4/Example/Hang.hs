-- | encoding for the "1:n" picture hanging problem
-- Section 3 of http://arxiv.org/abs/1203.3602

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module CO4.Example.Hang
where

import CO4
import CO4.Prelude
import Satchmo.Core.Decode
import CO4.Example.HangStandalone

$( compileFile [ ImportPrelude
               -- , InstantiationDepth 20
               -- , Profile
               , Cache
               ] 
  "test/CO4/Example/HangStandalone.hs" )

-- |@result s n@ solves the picture hanging problem
-- with @s@ being the no. of pins and @n@ the word length.
-- Successful example calls:
-- @result 2 4, result 3 10, result 4 16, result 5 28@.
-- The challenge is to find a solution for @s@
-- that is shorter than @bestknown s@ (see below)
-- or prove that it's impossible.
result :: Integer -> Int -> IO (Maybe (Hang,[Pin]))
result s n =
  let b = ceiling $ logBase 2 $ fromIntegral s
  in
    solveAndTestP 
      (nat b $ s - 1)
      (knownTuple2 (kList n $ knownTurn complete $ uNat b)
                   (allocatorList $ map (\p -> fromKnown $ nat b p) [0..s-1]) )
      encConstraint 
      constraint

bestknown s =
  if s == 1 then 1
  else let h = div s 2 in 2 * ( bestknown h + bestknown (s - h))
