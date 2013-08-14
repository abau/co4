{-# LANGUAGE TemplateHaskell #-}
module CO4.CodeGen.DecodeInstance
  (decodeInstance)
where

import           Control.Monad (forM)
import qualified Language.Haskell.TH as TH
import           Satchmo.Core.Decode (Decode,decode)
import           CO4.Names
import           CO4.Unique
import           CO4.Language
import           CO4.THUtil
import           CO4.EncodedAdt (EncodedAdt,IntermediateAdt(..),toIntermediateAdt)
import           CO4.Monad (SAT)

-- |Generates a @Decode@ instance of an ADT
-- 
-- > instance (Decode SAT EncodedAdt v1, ...) 
-- >  => Decode SAT (e p) (T v1 v2 ...) where
-- >    decode p = do
-- >      i <- toIntermediateAdt p <#constructors of Type>
-- >      case i of
-- >        IntermediateUndefined                 -> error "..."
-- >        IntermediateEmpty                     -> error "..."
-- >        IntermediateConstructorIndex 0 <args> -> do
-- >          p0 <- decode arg0
-- >          p1 <- decode arg1
-- >          ...
-- >          return (<Cons0> p0 p1 ...)
-- >        IntermediateConstructorIndex 1 <args> -> 
-- >        ...
-- >        IntermediateConstructorIndex d _      -> error "..."
-- 
decodeInstance :: MonadUnique u => Declaration -> u TH.Dec
decodeInstance (DAdt name vars conss) = do
  paramName        <- newName "d"
  intermediateName <- newName "i"

  let predicates = map (\v -> TH.ClassP ''Decode [ TH.ConT ''SAT
                                                 , TH.ConT ''EncodedAdt
                                                 , varT v]) vars

      instanceHead = TH.InstanceD predicates (foldl1 TH.AppT 
                      [ TH.ConT ''Decode, TH.ConT ''SAT, TH.ConT ''EncodedAdt
                      , appsT (conT name) $ map varT vars
                      ])
      instanceDec matches = funD "decode"
                              [TH.Clause [varP paramName]
                                         (TH.NormalB $ instanceExp matches) []
                              ]

      instanceExp matches = 
        TH.DoE [ TH.BindS (varP intermediateName) $
                  appsE (TH.VarE 'toIntermediateAdt) [ varE paramName
                                                     , intE $ length conss
                                                     ]
               , TH.NoBindS $ TH.CaseE (varE intermediateName) matches
               ]
  matches  <- forM (zip [0..] conss) $ uncurry decodeCons
  matchDef <- matchDefault name

  return $ instanceHead [ instanceDec $ concat [ [ matchUndefined name
                                                 , matchEmpty     name
                                                 ]
                                               ,  matches
                                               , [matchDef] ] ]

matchUndefined :: UntypedName -> TH.Match
matchUndefined adtName = 
  TH.Match (TH.ConP 'IntermediateUndefined [])
           (TH.NormalB $ TH.AppE (TH.VarE 'error)
                                 (TH.LitE $ TH.StringL 
                                          $ "Can not decode 'undefined' to data of type '" ++ fromName adtName ++ "'"
                                 )
           ) []

matchEmpty :: UntypedName -> TH.Match
matchEmpty adtName = 
  TH.Match (TH.ConP 'IntermediateEmpty [])
           (TH.NormalB $ TH.AppE (TH.VarE 'error)
                                 (TH.LitE $ TH.StringL 
                                          $ "Can not decode 'empty' to data of type '" ++ fromName adtName ++ "'"
                                 )
           ) []

matchDefault :: MonadUnique u => UntypedName -> u TH.Match
matchDefault adtName = do
  indexName <- newName "x"
  return $
    TH.Match (TH.ConP 'IntermediateConstructorIndex [varP indexName, TH.WildP])
             (TH.NormalB $ TH.AppE 
                (TH.VarE 'error)
                (TH.AppE 
                   (TH.VarE 'concat)
                   (TH.ListE 
                     [ TH.LitE $ TH.StringL $ "Can not decode constructor #" 
                     , TH.AppE (TH.VarE 'show) (varE indexName)
                     , TH.LitE $ TH.StringL $ " of type '" ++ fromName adtName ++ "'"
                     ]
                   )
                )
             ) []
  
decodeCons :: MonadUnique u => Int -> Constructor -> u TH.Match
decodeCons i (CCon consName params) = do
  paramNames   <- forM params $ const $ newName "p"
  decodedNames <- forM params $ const $ newName "d"

  let decodeBind (param,name) =   TH.BindS (varP name)
                                $ TH.AppE (TH.VarE 'decode) $ varE param

      matchPattern = 
        let paramPattern = foldr (\x y -> TH.ConP '(:) [x,y]) TH.WildP 
                         $ map varP paramNames
        in
          TH.ConP 'IntermediateConstructorIndex
                               [ intP $ fromIntegral i
                               , paramPattern
                               ]
      applyCons = foldl TH.AppE (conE consName) $ map varE decodedNames
      matchExp  = TH.DoE $ map decodeBind (zip paramNames decodedNames)
                      ++ [ TH.NoBindS $ TH.AppE (TH.VarE 'return) applyCons ]
  
  return $ TH.Match matchPattern (TH.NormalB matchExp) []
