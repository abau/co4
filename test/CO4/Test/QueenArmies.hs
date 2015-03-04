{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module CO4.Test.QueenArmies
where

import           Prelude hiding (length)
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude

$( [d|  
        type Color = Bool
        data Field = Empty | Occupied Color deriving Show
        type Board = [[Field]]
        data UNat  = Z | S UNat

        constraint :: UNat -> Board -> Bool
        constraint p board = and [ horizontallySafe     board
                                 , verticallySafe       board
                                 , digonallySafe        board
                                 , counterDigonallySafe board
                                 , sufficient         p board
                                 ]

        blackField :: Field -> Bool
        blackField f = case f of
          Empty      -> False
          Occupied c -> not c

        whiteField :: Field -> Bool
        whiteField f = case f of
          Empty      -> False
          Occupied c -> c

        horizontallySafe :: Board -> Bool
        horizontallySafe board = 
          all (\row -> ((not $ any blackField row) || (not $ any whiteField row))) board
      
        verticallySafe :: Board -> Bool
        verticallySafe board = horizontallySafe $ transpose board

        digonallySafe :: Board -> Bool
        digonallySafe board = horizontallySafe $ diagonals board

        counterDigonallySafe :: Board -> Bool
        counterDigonallySafe board = horizontallySafe $ counterDiagonals board

        sufficient :: UNat -> Board -> Bool
        sufficient p board = 
          let numBlack = length $ filter blackField $ concat board
              numWhite = length $ filter whiteField $ concat board
          in
            (eqUNat numBlack numWhite) && (eqUNat p numBlack)

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

        length :: [a] -> UNat
        length xs = S (maxIndex xs)

        eqUNat :: UNat -> UNat -> Bool
        eqUNat a b = case a of
          Z -> case b of Z -> True
                         _ -> False
          S a' -> case b of Z    -> False
                            S b' -> eqUNat a' b'

   |] >>= compile [ImportPrelude]
  )

kUNat 0 = Z
kUNat i = S $ kUNat $ i - 1

result :: Int -> Int -> IO ()
result armySize boardSize = do
  result <- solveAndTestP (kUNat armySize) 
                          (kList boardSize $ kList boardSize complete) 
                          encConstraint 
                          constraint
  case result of
    Nothing -> putStrLn "Nothing"
    Just b  -> putStrLn $ showBoard b

showBoard :: Board -> String
showBoard = unlines . map (unwords . map showField)
  where
    showField Empty            = "."
    showField (Occupied False) = "B"
    showField (Occupied True ) = "W"
