{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import           Prelude hiding (all,(!!),foldr1,foldr,tail,and, zipWith, or ,and,map,head,null)
import           Control.Monad (forM)
import           Control.Monad.Identity (runIdentity)
import           Satchmo.Boolean (boolean,assert)
import           Satchmo.Code (decode)
import           Satchmo.SAT.Mini (solve)
import qualified Language.Haskell.TH as TH
import           CO4
import qualified CO4.MonadifyTypes

$([d|
    
    simple_constraint bs =  
      let bla = True 
      in
        -- head ( map head [bs] ) && True
        head ( map (\b -> b || bla) bs )

    map f xs = case xs of [] -> [] ; x : xs -> f x : map f xs          
    head (x:xs) = x
  |] >>= \p -> TH.runIO ( compile p [Verbose, Degree 2] )
  )

testSimple :: IO ()
testSimple = 
  let test arg = runIdentity ( simple_constraint arg ) :: Bool
  in do
    mResult <- solve ( do bs <- forM [1..5] $ const boolean

                          r <- simple_constraint bs
                          assert [r]
                          return ( decode bs )
                     ) :: IO (Maybe [Bool])
    Prelude.putStrLn (unlines
      [ "Test:"
      , show (test `fmap` mResult)
      , show mResult
      ] )
