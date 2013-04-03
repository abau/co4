{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module CO4.Algorithms.Eitherize
  (eitherize)
where

import           Prelude hiding (undefined)
import           Control.Monad.Identity
import           Control.Monad.Reader
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
import           CO4.EncodedAdt 
  (undefined,isUndefined,encodedConsCall,caseOf,constructorArgument)

noEitherize :: Namelike a => a -> Bool
noEitherize a = "Param" `isPrefixOf` (fromName a)

noEitherizeScheme :: Scheme -> Bool
noEitherizeScheme (SType (TCon n [])) = noEitherize n
noEitherizeScheme _                   = False

encodedConsName :: Namelike a => a -> a
encodedConsName = mapName (\(n:ns) -> "enc" ++ (toUpper n : ns) ++ "Cons") 

encodedName :: Namelike a => a -> a
encodedName = mapName (\(n:ns) -> "enc" ++ (toUpper n : ns)) 

newtype AdtInstantiator u a = AdtInstantiator 
  { runAdtInstantiator :: WriterT [TH.Dec] u a } 
  deriving (Monad, MonadWriter [TH.Dec], MonadUnique)

instance MonadUnique u => MonadCollector (AdtInstantiator u) where

  collectAdt (DAdt name [] _) | noEitherize name = return ()

  collectAdt adt = do
    forM_ (zip [0..] $ dAdtConstructors adt) mkEncodedConstructor
    mkDecodeInstance

    where 
      mkDecodeInstance = decodeInstance adt >>= tellOne

      mkEncodedConstructor (i,CCon name []) = 
        let exp = returnE $ appsE (TH.VarE 'encodedConsCall) 
                                  [ intE i
                                  , intE $ length $ dAdtConstructors adt
                                  , TH.ListE [] ]
        in
          tellOne $ valD' (encodedConsName name) exp

      mkEncodedConstructor (i,CCon name args) = do
        paramNames <- forM args $ const $ newName ""

        let exp = returnE $ appsE (TH.VarE 'encodedConsCall) 
                                  [ intE i
                                  , intE $ length $ dAdtConstructors adt
                                  , TH.ListE $ map varE paramNames ]
        tellOne $ valD' (encodedConsName name) $ lamE' paramNames exp

      tellOne x    = tell [x]

type ToplevelName = Name

newtype ExpInstantiator u a = ExpInstantiator 
  { runExpInstantiator :: ReaderT [ToplevelName] u a } 
  deriving (Monad, MonadUnique, MonadReader [ToplevelName])

isToplevelName :: Monad u => ToplevelName -> ExpInstantiator u Bool
isToplevelName = asks . elem 

instance MonadUnique u => MonadTHInstantiator (ExpInstantiator u) where
  
  instantiateName = return . convertName . encodedName

  instantiateVar (EVar n) = isToplevelName n >>= \case
    True  -> liftM (                            TH.VarE) $ instantiateName n 
    False -> liftM (TH.AppE (TH.VarE 'return) . TH.VarE) $ instantiateName n

  instantiateCon (ECon n) = return $ varE $ encodedConsName n

  instantiateApp (EApp f args) = do
    args'    <- instantiate args
    case f of
      EVar fName -> instantiateApplication (encodedName     fName) args'
      ECon cName -> instantiateApplication (encodedConsName cName) args'
      _ ->  
       error $ unwords [ "instantiateApp", show f ]


    where instantiateApplication f' = bindAndApplyArgs (appsE $ varE f') 

  instantiateUndefined = return $ returnE $ TH.VarE 'undefined

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

          else do caseOfE <- bindAndApply (\ms'Names -> [ varE e'Name
                                                        , TH.ListE $ map varE ms'Names
                                                        ])
                                          (appsE $ TH.VarE 'caseOf) ms'

                  return $ TH.DoE [ binding, TH.NoBindS $ checkUndefined e'Name 
                                                        $ caseOfE ]
    where 
      -- |If the matched constructor has no arguments, just instantiate expression of match
      instantiateMatchToExp _ (_, Match (PCon _ []) match) = instantiate match

      -- |Otherwise, bind the constructor's arguments to new names
      instantiateMatchToExp e'Name (j, Match (PCon _ patVars) match) = do
        bindings' <- mapM mkBinding $ zip [0..] patVarNames
        match'    <- instantiate match 
        return $ letE' bindings' match'
        where 
          mkBinding (ith,var) = do 
            var' <- instantiateName var 
            return (var', eConstructorArg ith)

          eConstructorArg i   = appsE (TH.VarE 'constructorArgument) 
                                      [ intE i, intE j, varE e'Name ]

          patVarNames         = map (\(PVar n) -> nUntyped n) patVars

      -- |Instantiate default match (pattern is a variable)
      instantiateMatchToExp e'Name (_, Match (PVar v) match) = 
        liftM (letE' [(v, varE e'Name)]) $ instantiate match

      checkUndefined e'Name = 
          TH.CondE (TH.AppE (TH.VarE 'isUndefined) (varE e'Name))
                   (TH.AppE (TH.VarE 'return) (varE e'Name))

  instantiateLet (ELet bindings exp) = do
    exp'      <- instantiate exp 
    bindings' <- mapM bindValue bindings
    return $ TH.DoE $ bindings' ++ [ TH.NoBindS exp' ]

    where 
      bindValue (Binding name value) = do
        name'  <- instantiateName name
        value' <- instantiate value
        return $ bindS' name' value'

  instantiateBind (DBind (Binding name exp)) = do
    name' <- instantiateName name
    exp'  <- instantiate exp
    return [valD' name' exp']

-- |Passed program must be first order.
-- Note that @eitherize@ produces a Template-Haskell program
eitherize :: MonadUnique u => Program -> u [TH.Dec]
eitherize program = do
  typedProgram <- schemes program
  let (adts,values) = splitDeclarations typedProgram 
      toplevelNames = map boundName $ programToplevelBindings typedProgram

  decls   <- execWriterT $ runAdtInstantiator $ collect     adts
  values' <- liftM concat $ runReaderT (runExpInstantiator $ instantiate values)
                                       toplevelNames
                         
  return $ decls ++ (deleteSignatures values')


-- |@bindAndApply mapping f args@ binds @args@ to new names @ns@, maps $ns$ to 
-- expressions @es@ by @mapping@, applies @f@ to @es@ and
-- binds the result to a new name @r@. The last statement is @return r@.
bindAndApply :: MonadUnique u => ([Name] -> [TH.Exp]) -> ([TH.Exp] -> TH.Exp) 
                              -> [TH.Exp] -> u TH.Exp
bindAndApply mapping f args = do
  resultName <- newName "bindResult"
  argNames   <- forM args $ const $ newName "bindArgument"

  let bindings     = map (\(n,e) -> TH.BindS (varP n) e) $ zip argNames args
      applied      = f $ mapping argNames
      returnResult = [ TH.BindS     (varP resultName) applied
                     , TH.NoBindS $ returnE $ varE resultName
                     ]
  return $ TH.DoE $ bindings ++ returnResult

-- |@bindAndApplyArgs f args@ binds @args@ to new names @ns@,
-- applies @f@ to @ns@ and binds the result to a new name @r@. 
-- The last statement is @return r@.
bindAndApplyArgs :: MonadUnique u => ([TH.Exp] -> TH.Exp) 
                                  -> [TH.Exp] -> u TH.Exp
bindAndApplyArgs = bindAndApply (map varE) 
