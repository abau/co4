{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import           Prelude hiding (all,(!!),foldl,foldl1,foldr1,foldr,tail,and,zipWith,or,and,map,head,null)
import qualified Prelude as P
import           Control.Monad (forM,replicateM)
import           Control.Monad.Identity (runIdentity)
import           Satchmo.Boolean (Boolean,boolean,assert)
import           Satchmo.Code (decode)
import           Satchmo.SAT.Mini (SAT,solve)
import qualified Language.Haskell.TH as TH
import           CO4
import qualified CO4.MonadifyTypes

$([d|   
      constraint a b c = 
        foldl mtimes a [b,a,a] `mgreater` foldl mtimes c [b,a,b,a] && foldl mtimes a [c,b] `mgreater` foldl mtimes a [a,b,c,b,a] 
          && mvalid a && mvalid b && mvalid c

      -- Matrices
      mgreater a b = ands ( for2  a b ( \ r s -> 
          ands ( for2 r s ( \ x y -> fgreater x y ))))

      mtimes a b = for a ( \ row -> 
          for (transpose b) ( \ col -> 
          mdot row col))

      mdot xs ys = foldl uplus (        utimes (head xs) (head ys))
                               (zipWith utimes (tail xs) (tail ys))

      mvalid a = ( not ( infinite ( head ( head a ))))
            && ( ands ( for a ( \ row -> ands ( for row ( \ x -> uvalid x )))))

      infinite xs = head xs
      fgreater xs ys = infinite xs || ugreater xs ys

      -- Unaries
      uvalid xs = ands ( zipWith boolean_geq ( tail xs ) xs)

      umin xs ys = zipWith ( && ) xs ys
      uplus xs ys = zipWith ( && ) xs ys
      umax xs ys = zipWith ( || ) xs ys
      utimes xs ys = zipWith ( || ) xs ys

      uequal xs ys = ands ( zipWith boolean_eq xs ys )

      ugreater xs ys = ors ( zipWith  boolean_gt xs ys ) 

      all f xs = ands (map f xs)
      ands xs  = foldl (&&) True xs
      ors  xs  = foldl (||) False xs

      boolean_geq x y = x || not y
      boolean_eq x y = (x && y) || (not x && not y)
      boolean_gt x y = x && not y

      head (x:xs) = x 
      tail (x:xs) = xs   
      null xs = case xs of [] -> True ; _ : _ -> False
        
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

  |] >>= \p -> TH.runIO $ compile p [ Verbose, Metric "heap-space", Degree 5
                                    , DumpRaml "unary.raml"]
  )

testSimple :: IO ()
testSimple = 
  let unknowns dim bits = 
	replicateM dim $ replicateM dim $ replicateM bits boolean :: SAT [[[Boolean]]]
      test [a,b,c] = runIdentity ( constraint a b c ) :: Bool
  in do
    mResult <- solve ( do [a,b,c] <- forM [1..3] $ const $ unknowns 4 5
                          r <- constraint a b c
                          assert [r]
                          return ( decode [a,b,c] )
                   ) :: IO (Maybe [[[[Bool]]]])
    putStrLn (unwords 
      [ "Test:"
      , show (test `fmap` mResult)
      , show mResult
      ] )
