{-# LANGUAGE QuasiQuotes #-}
module CO4.Prelude
  ( parsePrelude, preludeAdtDeclarations, unparsedNames, unparsedPreludeContext
  , uBool, uList, kList, uTuple2, uTuple3, uTuple4, uTuple5
  , assertKnown, encAssertKnownProf, encAssertKnown
  , assertDefined, encAssertDefined, encAssertDefinedProf
  , module CO4.PreludeNat
  , module CO4.PreludeBool
  , module CO4.EncEq
  )
where

import qualified Language.Haskell.Exts as HE
import           Language.Haskell.Exts.QQ (dec)
import           CO4.Language 
import           CO4.Algorithms.HindleyMilner.Util (Context,binds,emptyContext,toList)
import           CO4.TypesUtil (functionType)
import           CO4.Frontend.HaskellSrcExts (parsePreprocessedProgram)
import           CO4.Unique (MonadUnique)
import           CO4.Names
import           CO4.AllocatorData (constructors,known)
import           CO4.PreludeNat
import           CO4.EncEq
import           CO4.EncodedAdt 
  (EncodedAdt,isConstantlyDefined,isInvalid,constantConstructorIndex)
import           CO4.Monad (CO4,traced)
import           CO4.PreludeBool

-- |Parses prelude's function definitions
parsePrelude :: MonadUnique u => u [Declaration]
parsePrelude = do
  Program _ decs <- parsePreprocessedProgram $ HE.Module 
                      (HE.SrcLoc "CO4Prelude" 0 0) (HE.ModuleName "CO4Prelude")
                      [] Nothing Nothing [] (main : preludeFunctionDeclarations)
  return decs
  where
    -- because parsePreprocessedProgram needs a main function:
    main = [dec| main = undefined |] 

-- The prelude may only contain definitions that are present in the Haskell prelude.
-- These declarations are parsed by CO4.
preludeFunctionDeclarations :: [HE.Decl]
preludeFunctionDeclarations = [
  -- Lists
    [dec| map f xs = case xs of { [] -> [] ; y : ys -> (f y) : (map f ys) } |]
  , [dec| foldr n c xs = case xs of { [] -> c ; y : ys -> n y (foldr n c ys) } |]
  , [dec| foldl n c xs = case xs of { [] -> c ; y : ys -> foldl n (n c y) ys } |]
  , [dec| a ++ b = foldr (:) b a |]
  , [dec| null xs = case xs of { [] -> True; _ -> False } |]
  , [dec| head xs = case xs of { [] -> undefined; (y:_) -> y } |]
  , [dec| tail xs = case xs of { [] -> []; (_:ys) -> ys } |]
  , [dec| last xs = case xs of { [] -> undefined ; x : ys -> case ys of { [] ->  x ; _  -> last ys } } |]
  , [dec| init xs = case xs of { [] -> undefined ; x : ys -> case ys of { [] -> [] ; _  -> x : init ys } } |]
  -- Functions
  , [dec| id x = x |]
  , [dec| const x y = x |]
  -- Booleans
  -- , [dec| not x    = case x of { False -> True ; True -> False } |]
  -- , [dec| a && b   = case a of { False -> False ; True -> b } |]
  -- , [dec| a || b   = case a of { False -> b ; True -> True } |]
  , [dec| and xs   = foldl (&&) True xs |]
  , [dec| or  xs   = foldl (||) False xs |]
  , [dec| all f xs = and (map f xs) |]
  , [dec| any f xs = or  (map f xs) |]
  -- Tuples
  , [dec| fst (x,_) = x |]
  , [dec| snd (_,y) = y |]
  , [dec| zip xs ys = case xs of { [] -> []; u : us -> case ys of { [] -> []; v : vs -> (u,v) : (zip us vs) } } |]
  , [dec| zipWith f xs ys = map (\(x,y) -> f x y) (zip xs ys) |]
  , [dec| filter f xs = case xs of { [] -> [] ; (x:xs) -> let ys = filter f xs in case f x of { False -> ys ; True -> x : ys } }|]
  , [dec| concat = foldr (++) [] |]
  ]

-- These declarations are not parsed by CO4.
preludeAdtDeclarations :: [Declaration]
preludeAdtDeclarations = [
    DAdt (UntypedName "Bool") [] [ CCon (UntypedName "False") []
                                 , CCon (UntypedName "True")  []
                                 ]
  , DAdt listName [a] 
      [ CCon listName []
      , CCon consName [ TVar a , TCon listName [TVar a] ]
      ]
  , DAdt (tupleName 2) [a,b]       [ CCon (tupleName 2) $ map TVar [a,b]       ]
  , DAdt (tupleName 3) [a,b,c]     [ CCon (tupleName 3) $ map TVar [a,b,c]     ]
  , DAdt (tupleName 4) [a,b,c,d]   [ CCon (tupleName 4) $ map TVar [a,b,c,d]   ]
  , DAdt (tupleName 5) [a,b,c,d,e] [ CCon (tupleName 5) $ map TVar [a,b,c,d,e] ]
  ]
  where
    [a,b,c,d,e] = map UntypedName ["a","b","c","d","e"]

unparsedPreludeContext :: Context
unparsedPreludeContext = binds 
  [ (eqName          , SForall a $ SType $ functionType [TVar a,TVar a] boolT)
  , ("False"         ,             SType boolT)
  , ("True"          ,             SType boolT)
  , (listName        , SForall a $ SType listT)
  , (consName        , SForall a $ SType $ functionType [TVar a,listT] listT)
  , mkTuple 2        , mkTuple 3 , mkTuple 4 , mkTuple 5
  , (nat8Name        , SType $ functionType [TCon intName []] nat8T)
  , ("gtNat8"        , SType $ functionType [nat8T,nat8T] boolT)
  , ("geNat8"        , SType $ functionType [nat8T,nat8T] boolT)
  , ("eqNat8"        , SType $ functionType [nat8T,nat8T] boolT)
  , ("leNat8"        , SType $ functionType [nat8T,nat8T] boolT)
  , ("ltNat8"        , SType $ functionType [nat8T,nat8T] boolT)
  , ("maxNat8"       , SType $ functionType [nat8T,nat8T] nat8T)
  , ("minNat8"       , SType $ functionType [nat8T,nat8T] nat8T)
  , ("plusNat8"      , SType $ functionType [nat8T,nat8T] nat8T)
  , ("timesNat8"     , SType $ functionType [nat8T,nat8T] nat8T)
  , ("assertKnown"   , SForall a $ SType $ functionType [TVar a] $ TVar a)
  , ("assertDefined" , SForall a $ SType $ functionType [TVar a] $ TVar a)
  , ("&&"            , SType $ functionType [boolT,boolT] boolT)
  , ("||"            , SType $ functionType [boolT,boolT] boolT)
  , ("not"           , SType $ functionType [boolT] boolT)
  , ("xor2"          , SType $ functionType [boolT,boolT] boolT)
  ] emptyContext
  where 
    names@(a:_) = map UntypedName ["a","b","c","d","e"]
    boolT       = TCon boolName []
    listT       = TCon listName [TVar a]

    mkTuple i = 
      let type_ = functionType (map TVar $ take i names) 
                $ TCon (tupleName i) $ map TVar $ take i names
      in
        ( tupleName i, (foldr SForall (SType type_) $ take i names) )

    nat8T = TCon nat8TypeName []

unparsedNames :: Namelike n => [n]
unparsedNames = map (convertName . fst) $ toList $ unparsedPreludeContext 

-- * Allocators

uBool     = constructors [ Just [], Just [] ]
uList 0 _ = constructors [ Just [], Nothing ]
uList i a = constructors [ Just [], Just [ a, uList (i-1) a ] ]

kList 0 _ = known 0 2 []
kList i a = known 1 2 [a, kList (i-1) a]

uTuple2 a b       = constructors [ Just [a,b]       ]
uTuple3 a b c     = constructors [ Just [a,b,c]     ]
uTuple4 a b c d   = constructors [ Just [a,b,c,d]   ]
uTuple5 a b c d e = constructors [ Just [a,b,c,d,e] ]

-- * Utilities

assertKnown :: a -> a
assertKnown = id

encAssertKnown,encAssertKnownProf  :: EncodedAdt -> CO4 EncodedAdt
encAssertKnown e | isInvalid e = return e
encAssertKnown e = case constantConstructorIndex e of 
  Nothing -> error "Prelude.encAssertKnown: assertion 'assertKnown' failed"
  Just _  -> return e
encAssertKnownProf = traced "assertDefined" . encAssertKnown

assertDefined :: a -> a
assertDefined = id

encAssertDefined,encAssertDefinedProf  :: EncodedAdt -> CO4 EncodedAdt
encAssertDefined e = 
  if isConstantlyDefined e 
  then return e
  else error "Prelude.encAssertDefined: assertion 'assertDefined' failed"
encAssertDefinedProf = traced "assertDefined" . encAssertDefined
