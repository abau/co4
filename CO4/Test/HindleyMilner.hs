{-# LANGUAGE TemplateHaskell #-}
module CO4.Test.HindleyMilner where

import           Prelude hiding (catch)
import           Control.Exception
import           Control.Applicative ((<$>))
import           System.Exit (exitSuccess,exitFailure)
import qualified Data.Map as M
import           Language.Haskell.TH (Q,Dec,Exp,runQ)
import qualified Language.Haskell.TH as TH
import           CO4

main :: IO ()
main = do
  expResults <- mapM (\(i,e,s) -> testExp i e $ parseScheme s)
              [ (1 , [| \x -> x |],                           "forall a . a -> a")
              , (2 , [| [\x -> x, \y -> y] |],                "forall a . List (a -> a)")
              , (3 , [| [1,2,3] |],                           "List Int") 
              , (4 , [| [1, read "3"] |],                     "List Int") 
              , (5 , [| show (read "3") |],                   "List Char")
              , (6 , [| (1, read "2") |],                     "forall a . Tuple2 Int a")
              , (7,  [| let f = case True of True -> 1 ; False -> 2 in f |], "Int")
              , (8,  [| let f = 5 in f |],                    "Int")
              , (9,  [| let f = \x -> x in (f 1, f "2") |],   "Tuple2 Int (List Char)") 
              , (10, [| case 3 of 1 -> "foo" ; x -> show x |],"List Char") 
              , (11, [| \[x,y] -> x + y |],                   "List Int -> Int") 
              , (12, [| \x -> case x of [y] -> y + 1 |],      "List Int -> Int") 
              ]

  programResults <- 
      mapM (\(i,d,c) -> testProgram i d c)
        [ (100, [d| foo x = x |] , mk [("foo", "forall a . a -> a")]) 
        , (101, [d| or  zs = foldr (||) False zs
                      
                    foldr k z xs = 
                      let go bla = case bla of
                                [] -> z
                                (y:ys) -> k y (go ys)
                      in go xs
                |]
         , mk [ ("or", "List Bool -> Bool")
              , ("foldr", "forall a . forall b . (a -> b -> b) -> b -> List a -> b")
              ])
        ]
  if and $ expResults ++ programResults 
    then putStrLn "Run tests successfully" >> exitSuccess 
    else exitFailure

  where mk = map (\(n,s) -> (Name n, parseScheme s))

testExp :: Integer -> Q Exp -> Scheme -> IO Bool
testExp i thExpQ scheme = do
  thExp <- runQ thExpQ
  let run = runUnique $ parseExpression thExp >>= schemeOfExp prelude
  mType <- catch (Just <$> evaluate run)
                 (\msg -> putStrLn (unlines 
                      [ "Expressions test case: " ++ show i
                      , TH.pprint thExp
                      , "Expected: " ++ (show $ pprint scheme)
                      , "Error: "    ++ show (msg :: ErrorCall)
                      ]) >> return Nothing)

  case mType of
    Nothing      -> return False
    Just scheme' -> if fuzzyEqScheme scheme' scheme
                    then return True
                    else putStrLn (unlines 
                          [ "Expressions test case: " ++ show i
                          , TH.pprint thExp
                          , "Expected: " ++ (show $ pprint scheme)
                          , "Actually: " ++ (show $ pprint scheme')
                          ]) >> return False

testProgram :: Integer -> Q [Dec] -> [(Name,Scheme)] -> IO Bool
testProgram i thDeclsQ expected = do
  thDecls <- runQ thDeclsQ
  let run = runUnique $ parseProgram thDecls >>= uniqueNames >>= schemes prelude
  mProg <- catch (Just <$> evaluate run)
                 (\msg -> putStrLn (unlines 
                      [ "Declarations test case: " ++ show i
                      , "Expected: " ++ (show $ pprint $ gamma expected)
                      , "Error: "    ++ show (msg :: ErrorCall)
                      ]) >> return Nothing)

  case mProg of
    Nothing   -> return False
    Just prog -> if all (inferredInProgram prog) expected 
                 then return True
                 else putStrLn (unlines 
                        [ "Declarations test case: " ++ show i
                        , "Expected: " ++ (show $ pprint $ gamma expected)
                        , "Actually: " ++ (unlines $ map (show . pprint) 
                                                   $ topLevelNames prog)
                        ]) >> return False

  where
    inferredInProgram p (name,scheme) =
      case boundInProgram name p of
        Nothing -> False
        Just (DBind (TypedName _ s) _) -> fuzzyEqScheme s scheme

fuzzyEqScheme :: Scheme -> Scheme -> Bool
fuzzyEqScheme s1 s2 = case (s1,s2) of
  (SType t1, SType t2)           -> fuzzyEqType t1 t2
  (SForall _ s1', SForall _ s2') -> fuzzyEqScheme s1' s2'
  _                              -> False

fuzzyEqType :: Type -> Type -> Bool
fuzzyEqType t1 t2 = case (t1,t2) of
  (TVar _, TVar _)           -> True
  (TCon c1 ts1, TCon c2 ts2) | c1 == c2 -> all (uncurry fuzzyEqType) $ zip ts1 ts2
  _                          -> False
