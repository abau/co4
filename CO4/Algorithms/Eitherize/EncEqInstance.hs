{-# LANGUAGE TemplateHaskell #-}
module CO4.Algorithms.Eitherize.EncEqInstance
  (encEqInstance)
where

import           Prelude hiding (and)
import           Control.Monad (forM,zipWithM,liftM)
import qualified Language.Haskell.TH as TH
import           Satchmo.Core.Primitive (Primitive,constant,and)
import           CO4.Unique (MonadUnique,newName)
import           CO4.Language
import           CO4.Names (Namelike,fromName)
import           CO4.EncEq (EncEq (..),EncProfiledEq (..))
import           CO4.EncodedAdt 
import           CO4.Algorithms.THInstantiator (toTH)
import           CO4.THUtil
import           CO4.TypesUtil (typeOfAdt)
import           CO4.Util (replaceAt,for)
import           CO4.Config (MonadConfig,Config(Profile),is)
import           CO4.Profiling (traced)

-- |Generates a @EncEq@ instance
--
-- > instance (Primitive p,EncodedAdt e p,EncEq v1 e p,...) 
-- >  => EncEq (T v1 ...) e p where
-- >   encEqPrimitive _ x y | isInvalid x = return (constant False)
-- >   encEqPrimitive _ x y | isInvalid y = return (constant False)
-- >   encEqPrimitive _ x y = do
-- >     let xFlags = flags' x
-- >         yFlags = flags' y
-- >
-- >     eq00  <- encEqPrimitive (undefined :: T00) (constructorArgument 0 0 x) 
-- >                                                (constructorArgument 0 0 y)
-- >     eq10  <- encEqPrimitive (undefined :: T10) (constructorArgument 1 0 x) 
-- >                                                (constructorArgument 1 0 y)
-- >     ...
-- >     eq0   <- and [eq00, eq10, eq20, ...]
-- >     eq1   <- and [eq01, eq11, eq21, ...]
-- >     ...
-- >   
-- >     eqY0  <- caseOfBits yFlags [Just [eq0], Just [constant False], ...]
-- >     eqY1  <- caseOfBits yFlags [Just [constant False], Just [eq1], ...]
-- >     ...
-- >     r     <- caseOfBits xFlags [Just y0, Just y1, ...]
-- >     return (flags' r)
encEqInstance :: (MonadUnique u, MonadConfig u) => Declaration -> u TH.Dec
encEqInstance adt@(DAdt name vars conss) = do
  [x,y,p,e] <- mapM newName ["x","y","p","e"]
  profile   <- is Profile

  let instanceName = if profile then ''EncProfiledEq else ''EncEq
      predicates   = primitive : encAdt : encEqs
        where 
          encEqs       = for vars $ \v -> TH.ClassP instanceName 
                                                    [varT v, varT e, varT p]
          primitive    = TH.ClassP ''Primitive [varT p]
          encAdt       = TH.ClassP ''EncodedAdt [varT e, varT p]
      
      instanceHead = TH.InstanceD predicates 
                   $ appsT (TH.ConT instanceName) [ appsT (conT name) $ map varT vars
                                                  , varT e, varT p ]
  body <- encEqBody profile x y conss 

  let thType            = toTH $ typeOfAdt adt
      mkClause b        = TH.Clause [typedWildcard thType, varP x, varP y] b []
      constantFalse     = appsE' [TH.VarE 'return, TH.VarE 'constant] $ TH.ConE 'False
      mkInvalidClause n = mkClause $ TH.GuardedB 
        [( TH.NormalG $ TH.AppE (TH.VarE 'isInvalid) (varE n), constantFalse )]
      clauses = 
         [ mkInvalidClause x
         , mkInvalidClause y
         , mkClause $ TH.NormalB $
             case profile of 
               False -> body
               True  -> appsE (TH.VarE 'traced) [stringE $ "==_" ++ fromName name, body]
         ]
  if profile
    then return $ instanceHead [funD "encProfiledEqPrimitive" clauses]
    else return $ instanceHead [funD "encEqPrimitive" clauses]

encEqBody :: (MonadUnique u,Namelike n) => Bool -> n -> n -> [Constructor] -> u TH.Exp
encEqBody profile x y conss = do
  [xFlags,yFlags,r] <- mapM newName ["xFlags","yFlags","r"]
  (eqJNames, eqIJStmts) <- liftM unzip $ zipWithM eqConstructor [0..] conss

  eqYNames  <- forM conss $ const $ newName "eqY"

  let letStmts      = TH.LetS
                      [ valD' xFlags $ TH.AppE (TH.VarE 'flags') (varE x)
                      , valD' yFlags $ TH.AppE (TH.VarE 'flags') (varE y)
                      ]
      constantFalse = TH.AppE (TH.VarE 'constant) (TH.ConE 'False)
      just          = TH.AppE (TH.ConE 'Just) 
      singleton     = TH.ListE . return
      eqYStmt j n   = bindS' n $ appsE (TH.VarE 'caseOfBits) 
                               $ [varE yFlags, TH.ListE args]

        where args  = replaceAt j (just $ singleton $ eqJNames !! j) 
                    $ replicate (length conss) 
                    $ just $ singleton constantFalse

      eqYStmts      = zipWith eqYStmt [0..] eqYNames

      rStmt         = bindS' r $ appsE (TH.VarE 'caseOfBits) 
                    [ varE xFlags, TH.ListE $ map (just . varE) eqYNames ]
      result        = appsE' [TH.VarE 'return, TH.VarE 'head] $ varE r

  return $ TH.DoE $ concat 
    [ [letStmts] 
    , concat eqIJStmts
    , eqYStmts 
    , [rStmt,TH.NoBindS result]
    ]
  where 
    eqConstructor _ (CCon _ []) = return (constantTrue, [])
      where 
        constantTrue = TH.AppE (TH.VarE 'constant) (TH.ConE 'True)

    eqConstructor j (CCon _ args) = do
      eqIJNames <- forM args $ const $ newName "eq"
      eqJName   <- newName "eq"

      let encIJs = zipWith3 (\n i -> eqIJ n i j) eqIJNames [0..] args 

          eqJ    = bindS' eqJName $ TH.AppE (TH.VarE 'and) 
                                  $ TH.ListE $ map varE eqIJNames

      return (varE eqJName, encIJs ++ [eqJ])

    eqIJ name i j t = 
      let encEqName = if profile then 'encProfiledEqPrimitive else 'encEqPrimitive
      in
        bindS' name $ appsE (TH.VarE encEqName) 
            [ typedUndefined $ toTH t
            , appsE (TH.VarE 'constructorArgument) [intE i, intE j, varE x]
            , appsE (TH.VarE 'constructorArgument) [intE i, intE j, varE y]
            ]
