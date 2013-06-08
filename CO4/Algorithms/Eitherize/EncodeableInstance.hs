{-# LANGUAGE TemplateHaskell #-}
module CO4.Algorithms.Eitherize.EncodeableInstance
where

import           Control.Monad (forM)
import qualified Language.Haskell.TH as TH
import           Satchmo.Core.Primitive (Primitive)
import           CO4.Unique
import           CO4.Language
import           CO4.THUtil
import           CO4.Encodeable (Encodeable (..))
import           CO4.Util (for)
import           CO4.EncodedAdt (EncodedAdt)

-- |Generates a @Encodeable@ instance
--
-- > instance (Primitive p,EncodedAdt e,Encodeable v1,...) 
-- >  => Encodeable (T v1 ...) e p where
-- >    encodeConstant (Cons1 a1 ...) = encodedConstructor 0 n [ encodeConstant a1 ... ] 
-- >    encodeConstant (Cons2 a1 ...) = encodedConstructor 1 n [ encodeConstant a1 ... ] 
encodeableInstance :: MonadUnique u => Declaration -> u TH.Dec
encodeableInstance (DAdt name vars conss) = do
  p        <- newName "p"
  e        <- newName "e"
  varNames <- forM vars $ const $ newName "v"

  let predicates = primitive : encAdt : encodeables
        where
          primitive   = TH.ClassP ''Primitive  [varT p]
          encAdt      = TH.ClassP ''EncodedAdt [varT e, varT p]
          encodeables = for varNames $ \v -> 
                          TH.ClassP ''Encodeable [varT v, varT e, varT p]
      
      instanceHead = TH.InstanceD predicates 
                   $ appsT (TH.ConT ''Encodeable)
                           [ appsT (conT name) $ map varT varNames
                           , varT e, varT p ]

      clause (i,CCon conName conArgs) = do
        patternVars <- forM conArgs $ const $ newName "a"
        let pattern = conP conName $ map varP patternVars
            body    = appsE (varE "encodedConstructor")
                        [ intE i, intE $ length conss
                        , TH.ListE $ map ( TH.AppE (TH.VarE 'encodeConstant) . varE ) 
                                   $ patternVars
                        ]
        return $ TH.Clause [pattern] (TH.NormalB body) []

  clauses <- forM (zip [0..] conss) clause
  return $ instanceHead [funD "encodeConstant" clauses]
