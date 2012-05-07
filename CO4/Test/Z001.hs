{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import           Prelude hiding (all,(!!),foldl1,foldl,foldr1,foldr,tail,and, zipWith, or ,and,map,head,null)
import           Control.Monad (forM)
import           Control.Monad.Identity (runIdentity)
import           Satchmo.Boolean (boolean,assert)
import           Satchmo.Code (decode)
import           Satchmo.SAT.Mini (solve)
import qualified Language.Haskell.TH as TH
import           CO4
import qualified CO4.MonadifyTypes

$([d|   

      {-
      simple_constraint x = fuzzy_interpretation [ ( [0,0],[0,1,0] ) ] x

      z001_constraint x = fuzzy_interpretation [ ( [0,0,1,1],[1,1,1,0,0,0] ) ] x
      -}
      simple_constraint x = fuzzy_interpretation [ ( [False,False],[False,True,False] ) ] x

      -- type F = [ (SAT Boolean) ]
      -- type Matrix a = [[a]]

      -- fuzzy_interpretation :: SRS -> [ Matrix F ] -> Bool
      fuzzy_interpretation srs int = 
            and ( for int mvalid ) && ( and ( for srs ( \ ( lhs, rhs ) -> 
               let eval w = 
                    let ms = for w ( \ i -> int !! i )
                    in
                      foldl mtimes (head ms) (tail ms)
               in  mgreater ( eval lhs ) ( eval rhs )    
            )))       

      -- * the fuzzy semiring.
      -- meaning of ( replicate x False ++ replicate y True )
      -- is: if x == 0 then Plus_Infinite else Finite y

      fvalid xs = and ( zipWith boolean_geq ( tail xs ) xs)

      fplus xs ys = zipWith ( && ) xs ys
      ftimes xs ys = zipWith ( || ) xs ys

      infinite xs = head xs
      fgreater xs ys = infinite xs || or ( zipWith  boolean_gt xs ys ) 

      -- * matrices over this semiring 

      mvalid a = ( not ( infinite ( head ( head a ))))
            && ( and ( for a ( \ row -> and ( for row ( \ x -> fvalid x )))))

      mtimes a b = for a ( \ row -> 
          for (transpose b) ( \ col -> 
          dotproduct row col))

      dotproduct xs ys = foldl fplus (        ftimes (head xs) (head ys))
                                     (zipWith ftimes (tail xs) (tail ys))

      -- * comparison of matrices

      mgreater a b = and ( for2  a b ( \ r s -> 
          and ( for2 r s ( \ x y -> fgreater x y ))))

      and xs = foldl (&&) True xs
      or  xs = foldl (||) False xs

      boolean_geq x y = x || not y
      boolean_gt x y = x && not y

      head (x:xs) = x 
      tail (x:xs) = xs   
      null xs = case xs of [] -> True ; _ : _ -> False
        
      --xs !! k = if k Prelude.> 0 then tail xs !! ( k Prelude.- 1 ) else head xs
      xs !! k = if k then head ( tail xs ) else (head xs)

      flip f x y = f y x          

      map f xs = case xs of [] -> [] ; x : xs -> f x : map f xs          

      foldl k z xs =
        case xs of
                [] -> z
                (y:ys) -> foldl k (k z y) ys

      for xs f = map f xs
      for2 xs ys f = zipWith f xs ys
          
      zipWith f xs ys = case xs of
          [] -> []
          (x:xs) -> case ys of
                      []     -> []
                      (y:ys) -> f x y : zipWith f xs ys

      transpose m = case m of 
        []        -> []
        (xs:xss)  -> case split m of
                        (l,m') -> case m' of [] -> []
                                             (y:ys) -> l : (transpose (y:ys))

      split m = case m of
        [] -> ([],[])
        (l:ls) -> case l of [] -> ([],[])
                            (x:xs) -> case split ls of
                                        (ys,m') -> (x:ys,xs:m')
  |] >>= \p -> TH.runIO $ compile p [Verbose, Degree 5, DumpRaml "z001.raml"]
  )

main :: IO ()
main = testSimple

testSimple :: IO ()
testSimple = 
  let unknowns alpha dim bits = 
         forM [ 1 .. alpha ] ( \ a -> 
         forM [ 1 .. dim ] ( \ i ->      
         forM [ 1 .. dim ] ( \ j ->           
         forM [ 1 .. bits ] ( \ k -> boolean ))))
      test arg = runIdentity ( simple_constraint arg ) :: Bool
  in do
    mResult <- solve ( do u <- unknowns 2 3 3
                          r <- simple_constraint u
                          assert [r]
                          return ( decode u )
                   ) :: IO (Maybe [[[[Bool]]]])
    putStrLn (unwords 
      [ "Test:"
      , show (test `fmap` mResult)
      , show mResult
      ] )

