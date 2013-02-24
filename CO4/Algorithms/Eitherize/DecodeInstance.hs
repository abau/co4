{-# LANGUAGE TemplateHaskell #-}
module CO4.Algorithms.Eitherize.DecodeInstance
  (decodeInstance)
where

import           Control.Monad (forM)
import qualified Language.Haskell.TH as TH
import           Satchmo.Code (Decode,decode)
import           Satchmo.SAT.Mini (SAT)
import           CO4.Names
import           CO4.Unique
import           CO4.Language
import           CO4.THUtil
import           CO4.EncodedAdt (EncodedAdt,IntermediateAdt(..),toIntermediateAdt)

-- |Generates a @Decode@ instance of an ADT
-- 
-- > instance Decode SAT EncodedAdt <Type> where
-- >    decode p = do
-- >      i <- toIntermediateAdt p
-- >      case i of
-- >        IntermediateConstructorIndex 0 <args> -> do
-- >          p0 <- decode arg0
-- >          p1 <- decode arg1
-- >          ...
-- >          return (<Cons0> p0 p1 ...)
-- >        IntermediateConstructorIndex 1 <args> -> 
-- >        ...
-- 
decodeInstance :: MonadUnique u => Declaration -> u TH.Dec
decodeInstance (DAdt name vars conss) = do
  paramName        <- newName "p"
  intermediateName <- newName "i"

  let instancePredicates = 
        map (\v -> TH.ClassP ''Decode [TH.ConT ''SAT, TH.ConT ''EncodedAdt, varT v]
            ) vars
  
      instanceHead = TH.InstanceD instancePredicates (foldl1 TH.AppT 
                      [ TH.ConT ''Decode, TH.ConT ''SAT, TH.ConT ''EncodedAdt
                      , appsT (conT name) $ map varT vars
                      ])
      instanceDec matches = funD "decode"
                              [TH.Clause [varP paramName]
                                         (TH.NormalB $ instanceExp matches) []
                              ]

      instanceExp matches = 
        TH.DoE [ TH.BindS (varP intermediateName) 
                  (TH.AppE (TH.VarE 'toIntermediateAdt) $ varE paramName)
               , TH.NoBindS $ TH.CaseE (varE intermediateName) matches
               ]
  matches <- forM (zip [0..] conss) $ uncurry decodeCons
  return $ instanceHead [ instanceDec $ matchUndefined name : matches ]

matchUndefined :: UntypedName -> TH.Match
matchUndefined adtName = 
  TH.Match (TH.ConP 'IntermediateUndefined [])
           (TH.NormalB $ TH.AppE (TH.VarE 'error)
                                 (TH.LitE $ TH.StringL 
                                          $ "Can not decode 'undefined' to data of type '" ++ fromName adtName ++ "'"
                                 )
           ) []
  
decodeCons :: MonadUnique u => Int -> Constructor -> u TH.Match
decodeCons i (CCon consName params) = do
  paramNames   <- forM params $ const $ newName "p"
  decodedNames <- forM params $ const $ newName "d"

  let decodeBind (param,name) =   TH.BindS (varP name)
                                $ TH.AppE (TH.VarE 'decode) $ varE param

      matchPattern = TH.ConP 'IntermediateConstructorIndex
                             [ TH.LitP  $ TH.IntegerL $ fromIntegral i
                             , TH.ListP $ map varP paramNames
                             ]
      applyCons = foldl TH.AppE (conE consName) $ map varE decodedNames
      matchExp  = TH.DoE $ map decodeBind (zip paramNames decodedNames)
                      ++ [ TH.NoBindS $ TH.AppE (TH.VarE 'return) applyCons ]
  
  return $ TH.Match matchPattern (TH.NormalB matchExp) []
