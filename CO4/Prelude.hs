{-# LANGUAGE QuasiQuotes #-}
module CO4.Prelude
  ( parsePrelude, preludeAdtDeclarations, unparsedFunctionNames, unparsedPreludeContext
  , uBool, uList
  )
where

import qualified Language.Haskell.Exts as HE
import           Language.Haskell.Exts.QQ (dec)
import           CO4.Language 
import           CO4.Algorithms.HindleyMilner.Util (Context (..),binds,emptyContext)
import           CO4.TypesUtil (functionType)
import           CO4.Frontend.HaskellSrcExts (parsePreprocessedProgram)
import           CO4.Unique (MonadUnique)
import           CO4.Names (Namelike,listName,consName,eqName)
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
    [dec| map f xs = case xs of { [] -> [] ; (y:ys) -> (f y) : (map f ys) } |]
  , [dec| foldr n c xs = case xs of { [] -> c ; (y:ys) -> n y (foldr n c ys) } |]
  , [dec| foldl n c xs = case xs of { [] -> c ; (y:ys) -> foldl n (n c y) ys } |]

  , [dec| not x  = case x of { False -> True ; True -> False } |]
  , [dec| a && b = case a of { False -> False ; True -> b } |]
  , [dec| a || b = case a of { False -> not b ; True -> True } |]
  , [dec| and xs = foldl (&&) True xs |]
  , [dec| or  xs = foldl (&&) True xs |]
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
  ]
  where
    a = UntypedName "a"

unparsedFunctionNames :: Namelike n => [n]
unparsedFunctionNames = [eqName]

unparsedPreludeContext :: Context
unparsedPreludeContext = binds 
  [ (eqName   , SForall a $ SType $ functionType [TVar a,TVar a] (TCon bool []))
  , ("False"  ,             SType $ TCon bool [])
  , ("True"   ,             SType $ TCon bool [])
  , (listName , SForall a $ SType listType)
  , (consName , SForall a $ SType $ functionType [TVar a,listType] listType)
  ] emptyContext
  where a        = UntypedName "a"
        bool     = UntypedName "Bool"
        listType = TCon listName [TVar a]

-- * Allocators

uBool     = constructors [ Just [], Just [] ]
uList 0 _ = constructors [ Just [], Nothing ]
uList i a = constructors [ Just [], Just [ a, uList (i-1) a ] ]
