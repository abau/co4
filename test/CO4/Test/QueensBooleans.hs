{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module CO4.Test.QueensBooleans
where

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude

$( [d|  type Board = [[Bool]]
        data UNat  = Z | S UNat

        constraint :: Board -> Bool
        constraint board = and [ horizontallySafe     board
                               , verticallySafe       board
                               , digonallySafe        board
                               , counterDigonallySafe board
                               , sufficient           board
                               ]

        horizontallySafe :: Board -> Bool
        horizontallySafe board = all atmost1 board
      
        verticallySafe :: Board -> Bool
        verticallySafe board = horizontallySafe $ transpose board

        digonallySafe :: Board -> Bool
        digonallySafe board = horizontallySafe $ diagonals board

        counterDigonallySafe :: Board -> Bool
        counterDigonallySafe board = horizontallySafe $ counterDiagonals board

        sufficient :: Board -> Bool
        sufficient board = all or board

        atmost1 :: [Bool] -> Bool
        atmost1 xs = case xs of
          []   -> True
          y:ys -> case y of
                    False -> atmost1 ys
                    True  -> all not ys

        diagonals :: [[a]] -> [[a]]
        diagonals xss =
          let xss' = transpose xss
              go n = case n of
                Z    -> []
                S n' -> (diagonal n xss) : (diagonal n xss') : (go n')
          in
            (diagonal Z xss) : (go $ maxIndex xss)

        counterDiagonals :: [[a]] -> [[a]]
        counterDiagonals xss = diagonals $ map reverse xss

        diagonal :: UNat -> [[a]] -> [a]
        diagonal offset xss = concat 
                            $ fst 
                            $ foldl (\(ys,n) xs -> ((at xs n):ys, S n)) ([], offset) xss

        at :: [a] -> UNat -> [a]
        at xs n = case n of
          Z    -> case xs of []   -> []
                             y:ys -> [y]
          S n' -> case xs of []   -> []
                             y:ys -> at ys n'

        transpose :: [[a]] -> [[a]]
        transpose list = case list of
          [] -> []
          xxs:xss -> 
            case xxs of 
              []   -> transpose xss
              x:xs -> let hhead ys = case ys of []  -> []
                                                y:_ -> [y]
                          ttail ys = case ys of []  -> []
                                                _:y -> y
                      in
                        (x : (concatMap hhead xss)) : transpose (xs : (map ttail xss))

        maxIndex :: [a] -> UNat
        maxIndex xs = case xs of
          []   -> undefined
          y:ys -> case ys of
            []   -> Z
            z:zs -> S (maxIndex ys)

   |] >>= compile [ImportPrelude,Cache]
  )

result :: Int -> IO (Maybe Board)
result i = solveAndTest (kList i $ kList i complete) encConstraint constraint
