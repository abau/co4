{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module CO4.Algorithms.Eitherize
  (eitherize)
where

import           Control.Applicative ((<$>))
import           Control.Monad.Identity
import           Control.Monad.Writer
import           Data.List (isPrefixOf)
import           Data.Char (toUpper)
import qualified Language.Haskell.TH as TH
import           CO4.Language
import           CO4.Util
import           CO4.THUtil
import           CO4.Names 
import           CO4.Algorithms.THInstantiator
import           CO4.Algorithms.Collector
import           CO4.Unique
import           CO4.Backend (displayExpression)
import           CO4.Backend.TH ()
import           CO4.Algorithms.HindleyMilner (schemes,schemeOfExp)
import           CO4.Algorithms.Eitherize.DecodeInstance (decodeInstance)
import           CO4.Algorithms.Eitherize.SizedGadt (sizedGadts)
import           CO4.Algorithms.Eitherize.Util

noEitherize :: Namelike a => a -> Bool
noEitherize a = "Param" `isPrefixOf` (fromName a)

noEitherizeScheme :: Scheme -> Bool
noEitherizeScheme (SType (TCon n [])) = noEitherize n
noEitherizeScheme _                   = False

encodedConsCallName :: Namelike a => a -> a
encodedConsCallName = mapName (\(n:ns) -> "enc" ++ (toUpper n : ns) ++ "ConsCall") 

encodedFunName :: Namelike a => a -> a
encodedFunName = mapName (\(n:ns) -> "enc" ++ (toUpper n : ns)) 

newtype AdtInstantiator u a = AdtInstantiator 
  { runAdtInstantiator :: WriterT [TH.Dec] u a } 
  deriving ( Functor, Monad, MonadWriter [TH.Dec], MonadUnique)

instance MonadUnique u => MonadCollector (AdtInstantiator u) where

  collectAdt (DAdt name [] _) | noEitherize name = return ()

  collectAdt adt = do
    forM_ (zip [0..] $ dAdtConstructors adt) mkEncodedConstructor
    mkDecodeInstance

    where 
      mkDecodeInstance = decodeInstance adt >>= tellOne

      mkEncodedConstructor (i,CCon name []) = 
        let args = replaceAt i (varE "encUnit") dontCareArgs
            exp  = returnE $ appsE (varE "encodedConsCall") [ intE i, TH.ListE args ]
        in
          tellOne $ valD' (encodedConsCallName name) exp

      mkEncodedConstructor (i,CCon name args) = do
        paramNames <- forM args $ const $ newName "x"

        let args = replaceAt i (appsE (varE "encArgs") [TH.ListE $ map varE paramNames])
                               dontCareArgs
            exp  = returnE $ appsE (varE "encodedConsCall") [ intE i, TH.ListE args ]
        
        tellOne $ valD' (encodedConsCallName name) $ lamE' paramNames exp

      dontCareArgs = replicate (length $ dAdtConstructors adt) $ varE "encDontCareArgs"
      tellOne x    = tell [x]

newtype ExpInstantiator u a = ExpInstantiator { runExpInstantiator :: u a } 
  deriving ( Functor, Monad, MonadUnique )

instance MonadUnique u => MonadTHInstantiator (ExpInstantiator u) where
  
  instantiateVar (EVar n) = return $ TH.AppE (TH.VarE 'return) $ varE n

  instantiateCon (ECon n) = return $ varE $ encodedConsCallName n

  instantiateApp (EApp f args) = do
    args'    <- instantiate args
    case f of
      EVar fName -> instantiateApplication (encodedFunName      fName) args'
      ECon cName -> instantiateApplication (encodedConsCallName cName) args'

    where instantiateApplication f' = bindAndApplyArgs (appsE $ varE f') 

  instantiateUndefined = return $ returnE $ varE "encUndefined"

  instantiateBinding (Binding name exp) = do
    exp' <- instantiate exp
    return [valD' (encodedFunName name) exp']

  instantiateCase (ECase e ms) = do
    eScheme <- schemeOfExp e
    if noEitherizeScheme eScheme
      then return (TH.CaseE (displayExpression e)) `ap` instantiate ms
      else do
        e'Name <- newName "bindCase"
        e'     <- instantiate e
        ms'    <- mapM (instantiateMatchToExp e'Name) $ zip [0..] ms

        let binding = bindS' e'Name e'

        if lengthOne ms'
          then return $ TH.DoE [ binding, TH.NoBindS $ head ms' ]

          else do switchByE <- bindAndApply (\ms'Names -> [ varE e'Name
                                                          , TH.ListE $ map varE ms'Names
                                                          ])
                                            (appsE $ varE "switchBy") ms'

                  return $ TH.DoE [ binding, TH.NoBindS $ dontCareMatch e'Name 
                                                        $ switchByE ]
    where 
      -- |If the matched constructor has no arguments, just instantiate expression of match
      instantiateMatchToExp _ (_, Match (PCon _ []) match) = instantiate match

      -- |Otherwise, bind the constructor's arguments to new names
      instantiateMatchToExp e'Name (j, Match (PCon _ patVars) match) = 
        lets <$> instantiate match
          
        where 
          lets                = letE' (map mkBinding $ zip [0..] patVarNames)
          mkBinding (ith,var) = (var, eConstructorArg ith)

          eConstructorArg i   = appsE (varE "constructorArgument") 
                                      [ intE i, intE j, varE e'Name ]

          patVarNames         = map (\(PVar n) -> nUntyped n) patVars

      dontCareMatch e'Name doCareBranch = TH.CaseE (varE e'Name)
          [ TH.Match (conP "EncDontCare" []) 
                     (TH.NormalB $ TH.AppE (TH.VarE 'return) (varE e'Name)) []
          , TH.Match (conP "EncUndefined" []) 
                     (TH.NormalB $ TH.AppE (TH.VarE 'return) (varE e'Name)) []
          , TH.Match TH.WildP (TH.NormalB doCareBranch) [] ]

  instantiateLet (ELet bindings exp) = do
    exp'      <- instantiate exp 
    bindings' <- mapM bindValue bindings
    return $ TH.DoE $ bindings' ++ [ TH.NoBindS exp' ]

    where bindValue (Binding name value) = return (bindS' name) `ap` instantiate value

-- |Passed program must be first order.
-- Note that @eitherize@ produces a Template-Haskell program
eitherize :: MonadUnique u => Program -> u [TH.Dec]
eitherize program = do
  typedProgram <- schemes program
  let (adts,values) = splitDeclarations typedProgram 

  gadts <- sizedGadts adts

  decls   <- execWriterT $ runAdtInstantiator $ collect     adts
  values' <- concat <$>  ( runExpInstantiator $ instantiate values )
  return $ gadts ++ decls ++ (deleteSignatures values')
