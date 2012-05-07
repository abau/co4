{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import           Prelude hiding (all,(!!),foldr1,foldr,tail,and, zipWith, or ,and,map,head,null)
import           Control.Monad (forM)
import           Control.Monad.Identity (runIdentity)
import           Satchmo.Boolean (boolean,assert)
import           Satchmo.Code (decode)
import           Satchmo.SAT.Mini (run)
import qualified Language.Haskell.TH as TH
import           CO4
import qualified CO4.MonadifyTypes

$([d|   
      mtimes a b = for a ( \ row -> 
          for (transpose b) ( \ col -> 
          dotproduct row col))

      dotproduct xs ys = foldr1 fplus ( zipWith ftimes xs ys )

      fplus xs ys = zipWith ( && ) xs ys
      ftimes xs ys = zipWith ( || ) xs ys

      and xs = foldr (&&) True xs
      or  xs = foldr (||) False xs

      boolean_geq x y = x || not y
      boolean_gt x y = x && not y

      head (x:xs) = x 
      tail (x:xs) = xs   
      null xs = case xs of [] -> True ; _ : _ -> False
        
      xs !! k = if k Prelude.> 0 then tail xs !! ( k Prelude.- 1 ) else head xs

      flip f x y = f y x          

      map f xs = case xs of [] -> [] ; x : xs -> f x : map f xs          

      foldr1 f list = case list of
                        (x:xs) -> 
                          case xs of 
                            []    -> x
                            (_:_) -> f x (foldr1 f xs)

      foldr k z xs =
        let go []     = z
            go (y:ys) = k y (go ys)
        in
          go xs

      for xs f = map f xs
      for2 xs ys f = zipWith f xs ys
          
      zipWith f xs ys = case xs of
          [] -> []
          (x:xs) -> case ys of
                      []     -> []
                      (y:ys) -> f x y : zipWith f xs ys

      transpose xss = 
          if null ( head xss ) then [] 
          else map head xss : transpose ( map tail xss )
  |] >>= \p -> TH.runIO $ compile p [Verbose, Degree 3]
  )

{-
testSimple :: IO ()
testSimple = 
  let unknowns alpha dim bits = 
         forM [ 1 .. alpha ] ( \ a -> 
         forM [ 1 .. dim ] ( \ i ->      
         forM [ 1 .. dim ] ( \ j ->           
         forM [ 1 .. bits ] ( \ k -> boolean ))))

      unknownNum bits = forM [ 1 .. bits ] $ const boolean
  in do
    mResult <- run ( do a <- unknownNum 5
                        b <- unknownNum 5
                        r <- fplus
                        assert [r]
                        return ( decode u )
                   ) :: IO (Maybe [[[[Bool]]]])
    putStrLn (unwords 
      [ "Test:"
      , show mResult
      ] )
-}
