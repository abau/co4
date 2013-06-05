{-# LANGUAGE QuasiQuotes #-}
module CO4.Prelude
  ( parsePrelude, preludeAdtDeclarations, unparsedFunctionNames, unparsedPreludeContext
  , uBool, uList, uTuple2, uTuple3, uTuple4, uTuple5
  )
where

import qualified Language.Haskell.Exts as HE
import           Language.Haskell.Exts.QQ (dec)
import           CO4.Language 
import           CO4.Algorithms.HindleyMilner.Util (Context (..),binds,emptyContext)
import           CO4.TypesUtil (functionType)
import           CO4.Frontend.HaskellSrcExts (parsePreprocessedProgram)
import           CO4.Unique (MonadUnique)
import           CO4.Names (Namelike,listName,consName,eqName,tupleName)
import           CO4.Allocator.Common (constructors)

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
    [dec| map f xs = case xs of { [] -> [] ; (y:ys) -> (f y) : (map f ys) } |]
  , [dec| foldr n c xs = case xs of { [] -> c ; (y:ys) -> n y (foldr n c ys) } |]
  , [dec| foldl n c xs = case xs of { [] -> c ; (y:ys) -> foldl n (n c y) ys } |]
  -- Booleans
  , [dec| not x  = case x of { False -> True ; True -> False } |]
  , [dec| a && b = case a of { False -> False ; True -> b } |]
  , [dec| a || b = case a of { False -> not b ; True -> True } |]
  , [dec| and xs = foldl (&&) True xs |]
  , [dec| or  xs = foldl (&&) True xs |]
  -- Tuples
  , [dec| fst (x,_) = x |]
  , [dec| snd (_,y) = y |]
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

unparsedFunctionNames :: Namelike n => [n]
unparsedFunctionNames = [eqName]

unparsedPreludeContext :: Context
unparsedPreludeContext = binds 
  [ (eqName   , SForall a $ SType $ functionType [TVar a,TVar a] (TCon bool []))
  , ("False"  ,             SType $ TCon bool [])
  , ("True"   ,             SType $ TCon bool [])
  , (listName , SForall a $ SType listType)
  , (consName , SForall a $ SType $ functionType [TVar a,listType] listType)
  , mkTuple 2 , mkTuple 3 , mkTuple 4 , mkTuple 5
  ] emptyContext
  where 
    names@(a:_) = map UntypedName ["a","b","c","d","e"]
    bool        = UntypedName "Bool"
    listType    = TCon listName [TVar a]

    mkTuple i = 
      let type_ = functionType (map TVar $ take i names) 
                $ TCon (tupleName i) $ map TVar $ take i names
      in
        ( tupleName i, (foldr SForall (SType type_) $ take i names) )

-- * Allocators

uBool     = constructors [ Just [], Just [] ]
uList 0 _ = constructors [ Just [], Nothing ]
uList i a = constructors [ Just [], Just [ a, uList (i-1) a ] ]

uTuple2 a b       = constructors [ Just [a,b]       ]
uTuple3 a b c     = constructors [ Just [a,b,c]     ]
uTuple4 a b c d   = constructors [ Just [a,b,c,d]   ]
uTuple5 a b c d e = constructors [ Just [a,b,c,d,e] ]
