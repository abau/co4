{-# LANGUAGE QuasiQuotes #-}
module CO4.Prelude
  ( parsePrelude, preludeAdtDeclarations, unparsedNames, unparsedPreludeContext
  , uBool, uList, kList, kNil, kList', kBool, kTuple2, kTuple3, kTuple4, kTuple5, kUnit
  , assertKnown, encAssertKnownProf, encAssertKnown
  , assertDefined, encAssertDefined, encAssertDefinedProf
  , dumpEncoded, encDumpEncoded, encDumpEncodedProf
  , module CO4.PreludeNat
  , module CO4.PreludeBool
  )
where

import qualified Language.Haskell.Exts as HE
import           Language.Haskell.Exts.QQ (dec)
import           Satchmo.Core.Primitive (isConstant)
import           Satchmo.Core.MonadSAT (note)
import           CO4.Language 
import           CO4.Algorithms.HindleyMilner.Util (Context,bind,emptyContext,toList)
import           CO4.TypesUtil (functionType)
import           CO4.Frontend.HaskellSrcExts (toTHDeclarations)
import           CO4.Frontend.TH (parsePreprocessedTHDeclarations)
import           CO4.Unique (MonadUnique)
import           CO4.Names
import           CO4.Allocator.Data (constructors,known)
import           CO4.PreludeNat
import           CO4.EncodedAdt (EncodedAdt,isConstantlyDefined,isInvalid,flags')
import           CO4.Monad (CO4,traced,abortWithStackTrace)
import           CO4.PreludeBool

-- |Parses prelude's function definitions
parsePrelude :: MonadUnique u => u [Declaration]
parsePrelude = do
  Program _ decs <- parsePreprocessedTHDeclarations
                  $ toTHDeclarations 
                  $ HE.Module (HE.SrcLoc "CO4Prelude" 0 0) (HE.ModuleName "CO4Prelude")
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
  , [dec| reverse xs = foldl (flip (:)) [] xs |]
  , [dec| null xs = case xs of { [] -> True; _ -> False } |]
  , [dec| head xs = case xs of { [] -> undefined; (y:_) -> y } |]
  , [dec| tail xs = case xs of { [] -> []; (_:ys) -> ys } |]
  , [dec| last xs = case xs of { [] -> undefined ; x : ys -> case ys of { [] ->  x ; _  -> last ys } } |]
  , [dec| init xs = case xs of { [] -> undefined ; x : ys -> case ys of { [] -> [] ; _  -> x : init ys } } |]
  , [dec| filter f xs = case xs of { [] -> [] ; (x:xs) -> let ys = filter f xs in case f x of { False -> ys ; True -> x : ys } }|]
  , [dec| concat = foldr (++) [] |]
  -- Functions
  , [dec| id x = x |]
  , [dec| const x y = x |]
  , [dec| flip f x y = f y x |]
  -- Booleans (are built-in)
  -- , [dec| not x    = case x of { False -> True ; True -> False } |]
  -- , [dec| a && b   = case a of { False -> False ; True -> b } |]
  -- , [dec| a || b   = case a of { False -> b ; True -> True } |]
  -- Tuples
  , [dec| fst (x,_) = x |]
  , [dec| snd (_,y) = y |]
  -- maybe, either
  , [dec| maybe nothing just m = case m of { Nothing -> nothing ; Just x -> just x } |]
  , [dec| either left right e = case e of { Left x -> left x ; Right y -> right y } |]
  -- more Lists
  , [dec| and xs   = foldl (&&) True xs |]
  , [dec| or  xs   = foldl (||) False xs |]
  , [dec| all f xs = and (map f xs) |]
  , [dec| any f xs = or  (map f xs) |]
  , [dec| zip xs ys = case xs of { [] -> []; u : us -> case ys of { [] -> []; v : vs -> (u,v) : (zip us vs) } } |]
  , [dec| zipWith f xs ys = map (\(x,y) -> f x y) (zip xs ys) |]
  , [dec| concatMap f xs = concat (map f xs) |]
  , [dec| unzip xs = foldr (\(u,v) (us,vs) -> (u:us, v:vs)) ([],[]) xs |]
  ]

-- These declarations are not parsed by CO4.
preludeAdtDeclarations :: [Adt]
preludeAdtDeclarations = [
    Adt boolName [] [ CCon (UntypedName "False") []
                    , CCon (UntypedName "True")  []
                    ]
  , Adt listName [a] 
     [ CCon nilName []
     , CCon consName [ TVar a , TCon listName [TVar a] ]
     ]
  , Adt (tupleTypeName 2) [a,b]       [ CCon (tupleDataName 2) $ map TVar [a,b]       ]
  , Adt (tupleTypeName 3) [a,b,c]     [ CCon (tupleDataName 3) $ map TVar [a,b,c]     ]
  , Adt (tupleTypeName 4) [a,b,c,d]   [ CCon (tupleDataName 4) $ map TVar [a,b,c,d]   ]
  , Adt (tupleTypeName 5) [a,b,c,d,e] [ CCon (tupleDataName 5) $ map TVar [a,b,c,d,e] ]

  , Adt maybeName [a] [ CCon ( readName "Nothing") []
                      , CCon ( readName "Just") $ map TVar [a]
                      ]
  , Adt eitherName [a, b] [ CCon ( readName "Left") [ TVar a ]
                      , CCon ( readName "Right") [ TVar b]
                      ]
  , Adt orderingName [] [ CCon (readName "LT") []
                        , CCon (readName "EQ") []
                        , CCon (readName "GT") [] 
                        ] 
  , Adt unitName [] [ CCon unitName [] ]
  ]
  where
    [a,b,c,d,e] = map UntypedName ["a","b","c","d","e"]

unparsedPreludeContext :: Context
unparsedPreludeContext = bind (
  [ (natName         , SType $ functionType [TCon intName [],TCon intName []] natT)
  , ("gtNat"         , SType $ functionType [natT,natT] boolT)
  , ("geNat"         , SType $ functionType [natT,natT] boolT)
  , ("eqNat"         , SType $ functionType [natT,natT] boolT)
  , ("leNat"         , SType $ functionType [natT,natT] boolT)
  , ("ltNat"         , SType $ functionType [natT,natT] boolT)
  , ("isZeroNat"     , SType $ functionType [natT] boolT)
  , ("maxNat"        , SType $ functionType [natT,natT] natT)
  , ("minNat"        , SType $ functionType [natT,natT] natT)
  , ("plusNat"       , SType $ functionType [natT,natT] natT)
  , ("plus'Nat"      , SType $ functionType [natT,natT] natT)
  , ("plusCLANat"    , SType $ functionType [natT,natT] natT)
  , ("plus'CLANat"   , SType $ functionType [natT,natT] natT)
  , ("timesNat"      , SType $ functionType [natT,natT] natT)
  , ("shiftLNat"     , SType $ functionType [natT] natT)
  , ("shiftRNat"     , SType $ functionType [natT] natT)
  , ("andNat"        , SType $ functionType [natT,natT] natT)
  , ("orNat"         , SType $ functionType [natT,natT] natT)
  , ("xorNat"        , SType $ functionType [natT,natT] natT)
  , ("assertKnown"   , SForall a $ SType $ functionType [TVar a] $ TVar a)
  , ("assertDefined" , SForall a $ SType $ functionType [TVar a] $ TVar a)
  , ("dumpEncoded"   , SForall a $ SType $ functionType [TVar a] $ TVar a)
  , ("&&"            , SType $ functionType [boolT,boolT] boolT)
  , ("||"            , SType $ functionType [boolT,boolT] boolT)
  , ("not"           , SType $ functionType [boolT] boolT)
  , ("xor2"          , SType $ functionType [boolT,boolT] boolT)
  ]
  ++ (concatMap fromAdt preludeAdtDeclarations)) emptyContext
  where 
    a     = UntypedName "a"
    boolT = TCon boolName []
    natT  = TCon natTypeName []

    fromAdt (Adt name vars conss) = map fromCons conss
      where
        mkScheme t                  = foldr SForall (SType t) vars
        resultT                     = TCon name $ map TVar vars
        fromCons (CCon cName argTs) = (fromName cName, mkScheme $ functionType argTs resultT)

unparsedNames :: Namelike n => [n]
unparsedNames = map (convertName . fst) $ toList $ unparsedPreludeContext 

-- * Allocators

uBool             = constructors [ Just [], Just [] ]
kBool False       = known 0 2 []
kBool True        = known 1 2 []

uList 0 _         = known 0 2 []
uList i a         = constructors [ Just [], Just [ a, uList (i-1) a ] ]
kList 0 _         = known 0 2 []
kList i a         = known 1 2 [a, kList (i-1) a]
kNil              = kList 0 undefined
kList'            = foldr (\elemAlloc listAlloc -> known 1 2 [elemAlloc, listAlloc]) kNil

kTuple2 a b       = constructors [ Just [a,b]       ]
kTuple3 a b c     = constructors [ Just [a,b,c]     ]
kTuple4 a b c d   = constructors [ Just [a,b,c,d]   ]
kTuple5 a b c d e = constructors [ Just [a,b,c,d,e] ]

kUnit             = known 0 1 []

-- * Utilities

assertKnown :: a -> a
assertKnown = id

encAssertKnown,encAssertKnownProf  :: EncodedAdt -> CO4 EncodedAdt
encAssertKnown e | isInvalid e = return e
encAssertKnown e = 
  if all isConstant (flags' e)
  then return e
  else abortWithStackTrace "Prelude.encAssertKnown: assertion 'assertKnown' failed" 
encAssertKnownProf = traced "assertKnown" . encAssertKnown

assertDefined :: a -> a
assertDefined = id

encAssertDefined,encAssertDefinedProf  :: EncodedAdt -> CO4 EncodedAdt
encAssertDefined e = 
  if isConstantlyDefined e 
  then return e
  else abortWithStackTrace "Prelude.encAssertDefined: assertion 'assertDefined' failed" 
encAssertDefinedProf = traced "assertDefined" . encAssertDefined

dumpEncoded :: a -> a
dumpEncoded = id

encDumpEncoded,encDumpEncodedProf :: EncodedAdt -> CO4 EncodedAdt
encDumpEncoded adt = do
  note $ "dumpEncoded: " ++ show adt
  return adt
encDumpEncodedProf = traced "dumpEncoded" . encDumpEncoded
