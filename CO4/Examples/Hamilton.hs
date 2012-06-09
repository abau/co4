{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import           Prelude hiding (all,(!!),foldl,foldl1,foldr1,foldr,tail,and,zipWith,or,and,map,head,null,snd,(++))
import qualified Prelude as P
import           Control.Monad (forM,replicateM)
import           Control.Monad.Identity (runIdentity)
import           Satchmo.Boolean (Boolean,boolean,assert)
import           Satchmo.Code (decode)
import           Satchmo.SAT.Mini (SAT,solve)
import qualified Data.Ix
import qualified Language.Haskell.TH as TH
import           CO4
import qualified CO4.MonadifyTypes

-- | the parameter is
-- the adjacency matrix (Relation) of a graph.
-- we expect entries from {0,1} 
-- but we don't use Bool since CO4 would encode that.
type Graph = [[ Int ]]

-- | the unknown. this should be encoded.
--  m !! i !! v  means that  m(i) = v
-- (the i-th node on the path is  v )
type Map = [[Bool]]

$([d|   
  constraint g f = bijective f && path g f

  nonzero e = if e /= 0 then True else False 
                   
  path g f = ands (for2 f (append (tail f) [ head f ]) 
                    ( connected g ))  

  connected g xs ys =            
     ands ( for2 xs g ( \ x row ->
     ands ( for2 ys row ( \ y  e ->
       implies (x && y) (nonzero e)))))

  bijective f = all exactly_one ( transpose f )
           && all exactly_one f     
     
  exactly_one xs = snd 
   ( foldr ( \ x (zero,one) -> (zero && not x 
                , one && not x || zero && x ) )  
       ( True, False ) xs )
              
  implies x y = not x || y 
  snd (x,y) = y 
  head (x:xs) = x            
  tail (x:xs) = xs 
  all f xs = ands (map f xs)
  ands    = foldr (&&) True 
  map f   = foldr ( \ x y -> f x : y ) []
  for2 xs ys f = case xs of
    [] -> []
    (x:xs) -> case ys of
      []     -> []
      (y:ys) -> f x y : for2 xs ys f

  append xs ys = foldr cons ys xs  
  cons x xs = (:) x xs  
             
  foldr k z xs = case xs of
                [] -> z
                (y : ys) -> k y ( foldr k z ys )

  transpose m = case m of 
    []        -> []
    (xs:xss)  -> case split m of
        (l,m') -> case m' of 
             [] -> []
             (y:ys) -> l : (transpose (y:ys))

  split m = case m of
    [] -> ([],[])
    (l:ls) -> case l of 
        [] -> ([],[])
        (x:xs) -> case split ls of
            (ys,m') -> (x:ys,xs:m')
  
 |]  >>= \ p -> 
    -- CO4 compilation:
    TH.runIO $ compile p [Verbose, Degree 3 , Metric "eval-steps"] 
    -- or comment our previous line and
    -- see that the original program typechecks:
    -- return p 
 )   


nodes h w =  Data.Ix.range ((1,1),(h,w))

knight h w = 
  for (nodes h w) $ \ (x1,y1) ->  
  for (nodes h w) $ \ (x2,y2) -> 
        if 5 == (x1-x2)^2 + (y1-y2)^2
        then 1 else 0   

for = flip P.map

main = do
    let h = 6 ; w = 6
        g = knight h w
    out :: Maybe Map <- solve $ do    
          f :: [[Boolean]] <- forM g   $ \ row -> 
                              forM row $ \ _ -> 
                              boolean
          r <- constraint g f
          assert [r]
          return $ decode f 
    case out of      
        Just m -> print $ for m $ \ xs ->
                         P.map P.snd
                       $ filter P.fst 
                       $ zip xs $ nodes h w 
