{-# LANGUAGE TemplateHaskell #-}
module CO4.CodeGen.EncodeableInstance
where

import           Control.Monad (forM)
import qualified Language.Haskell.TH as TH
import           CO4.Unique
import           CO4.Language
import           CO4.THUtil
import           CO4.Encodeable (Encodeable (..))
import           CO4.Util (for)

-- |Generates a @Encodeable@ instance
--
-- > instance (Encodeable v1,...) 
-- >  => Encodeable (T v1 ...) e p where
-- >    encode (Cons1 a1 ...) = 
-- >      e1 <- encode a1 
-- >      ...
-- >      encodedConstructor 0 n [ e1 ... ] 
-- >    encode (Cons2 a1 ...) = ...
encodeableInstance :: MonadUnique u => Adt -> u TH.Dec
encodeableInstance (Adt name vars conss) = do
  varNames <- forM vars $ const $ newName "v"

  let predicates = for varNames $ \v -> TH.ClassP ''Encodeable [varT v]
      instanceHead = TH.InstanceD predicates 
                   $ appsT (TH.ConT ''Encodeable) [appsT (conT name) $ map varT varNames]

      clause (i,CCon conName conArgs) = do
        patternVars <- forM conArgs $ const $ newName "a"
        encodeVars  <- forM conArgs $ const $ newName "e"

        let pattern = conP conName $ map varP patternVars
            body    = TH.DoE $ bindings ++ [encodedCons]
              where
                bindings = for (zip patternVars encodeVars) $ \(a,e) -> 
                  TH.BindS (varP e) $ TH.AppE (TH.VarE 'encode) $ varE a

                encodedCons = TH.NoBindS $ appsE (varE "encodedConstructor")
                        [ intE i
                        , intE $ length conss
                        , TH.ListE $ map varE encodeVars
                        ]
        return $ TH.Clause [pattern] (TH.NormalB body) []

  clauses <- forM (zip [0..] conss) clause
  return $ instanceHead [funD "encode" clauses]
