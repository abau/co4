{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# language LambdaCase #-}

module CO4.Algorithms.Eitherize
  (eitherize)
where

import           Prelude hiding (undefined)
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.List (find)
import qualified Language.Haskell.TH as TH
import           CO4.Language
import           CO4.Util
import           CO4.THUtil
import           CO4.Names 
import           CO4.Algorithms.THInstantiator
import           CO4.Algorithms.Collector
import           CO4.Unique
import           CO4.Algorithms.Eitherize.Names
import           CO4.Algorithms.Eitherize.DecodeInstance (decodeInstance)
import           CO4.Algorithms.Eitherize.EncodeableInstance (encodeableInstance)
import           CO4.Algorithms.Eitherize.EncEqInstance (encEqInstance)
import           CO4.EncodedAdt 
  (EncodedAdt,undefined,isInvalid,encodedConstructor,caseOf,constructorArgument)
import           CO4.Algorithms.HindleyMilner (schemes,schemeOfExp)
import           CO4.Monad (CO4,withCallCache,traced)
import           CO4.AllocatorData (known)
import           CO4.EncEq (encEq)
import           CO4.Config (MonadConfig,is,Config(ImportPrelude,Profile,Cache))
import           CO4.Prelude (preludeAdtDeclarations,unparsedNames) 

newtype AdtInstantiator u a = AdtInstantiator 
  { runAdtInstantiator :: WriterT [TH.Dec] u a } 
  deriving (Monad, MonadConfig, MonadWriter [TH.Dec], MonadUnique)

instance (MonadUnique u,MonadConfig u) => MonadCollector (AdtInstantiator u) where

  collectAdt adt = do
    forM_ (zip [0..] $ dAdtConstructors adt) $ \constructor -> do
      mkAllocator constructor
      mkEncodedConstructor constructor

    decodeInstance adt     >>= tellOne
    encodeableInstance adt >>= tellOne

    is ImportPrelude >>= \case
      True  -> encEqInstance adt >>= tellOne
      False -> return ()

    where 
      mkAllocator          = withConstructor allocatorName   'known
      mkEncodedConstructor = withConstructor encodedConsName 'encodedConstructor

      withConstructor bindTo callThis (i,CCon name args) = do
        paramNames <- forM args $ const $ newName ""

        let exp = appsE (TH.VarE callThis) 
                      [ intE i
                      , intE $ length $ dAdtConstructors adt
                      , TH.ListE $ map varE paramNames ]
        tellOne $ valD' (bindTo name) 
                $ if null args 
                  then exp
                  else lamE' paramNames exp

      tellOne x    = tell [x]

type ToplevelName = Name
data ExpInstantiatorData = ExpInstantiatorData 
  { toplevelNames :: [ToplevelName]
  , adts          :: [Declaration]
  }

newtype ExpInstantiator u a = ExpInstantiator 
  { runExpInstantiator :: ReaderT ExpInstantiatorData u a } 
  deriving (Monad, MonadUnique, MonadConfig, MonadReader ExpInstantiatorData)

isToplevelName :: Monad u => ToplevelName -> ExpInstantiator u Bool
isToplevelName name = asks $ elem name . toplevelNames

instance (MonadUnique u,MonadConfig u) => MonadTHInstantiator (ExpInstantiator u) where

  instantiateName n = is Profile >>= \case 
    False -> return $ convertName $ encodedName     n
    True  -> return $ convertName $ encodedNameProf n

  instantiateVar (EVar n) = isToplevelName n >>= \case
    True  -> liftM (                            TH.VarE) $ instantiateName n 
    False -> liftM (TH.AppE (TH.VarE 'return) . TH.VarE) $ instantiateName n

  instantiateCon (ECon n) = return $ varE $ encodedConsName n

  instantiateApp (EApp f args) = do
    args' <- instantiate args
    cache <- is Cache
    case f of
      ECon cName -> bindAndApplyArgs (appsE $ varE $ encodedConsName cName) args'
      EVar fName -> do
        fName' <- instantiateName fName
        case fromName fName of
          n | n == eqName   -> instantiateEq args'
          n | n == nat8Name -> case args of
            [ECon i] -> return $ TH.AppE (varE fName') $ intE $ read $ fromName i
            _        -> error $ "Algorithms.Eitherize.instantiateApp: nat8"

          _ | cache  -> bindAndApplyArgs (\args'' -> 
                          appsE (TH.VarE 'withCallCache) 
                          [ TH.TupE [ stringE fName', TH.ListE args'']
                          , appsE (varE fName') args''
                          ]) args'
          _         -> bindAndApplyArgs (appsE $ varE fName') args'
    where 
      instantiateEq args' = do
        scheme <- liftM toTH $ schemeOfExp $ head args

        is Cache >>= \case
          False -> bindAndApplyArgs (\args'' -> appsE (TH.VarE 'encEq) 
                                              $ typedUndefined scheme : args''
                                    ) args'

          True  -> bindAndApplyArgs (\args'' -> 
                    appsE (TH.VarE 'withCallCache) 
                    [ TH.TupE [stringE "==", TH.ListE args'']
                    , appsE (TH.VarE 'encEq) $ typedUndefined scheme : args''
                    ]) args'

  instantiateCase (ECase e ms) = do
    e'Name <- newName "bindCase"
    e'     <- instantiate e
    ms'    <- instantiateMatches e'Name ms

    let binding = bindS' e'Name e'

    if lengthOne ms'
      then return $ TH.DoE [ binding, TH.NoBindS $ head ms' ]

      else do 
        caseOfE <- bindAndApply 
                     (\ms'Names -> [ varE e'Name, TH.ListE $ map varE ms'Names ])
                     (appsE $ TH.VarE 'caseOf) ms'

        return $ TH.DoE [ binding, TH.NoBindS $ checkValidity e'Name $ caseOfE ]
    where 
      -- Instantiate matches
      instantiateMatches e'Name matches =
        getAdt >>= \case 
          Nothing  -> error "Algorithms.Eitherize.instantiateMatches: no ADT found"
          Just adt -> zipWithM instantiateMatch [0..] $ dAdtConstructors adt
        
        where
          -- Default match
          defaultMatch = case last matches of m@(Match (PVar _) _) -> Just m
                                              _                    -> Nothing

          -- Instantiate match of @j@-th constructor, namely @CCon c _@
          instantiateMatch j (CCon c _) = case matchFromConstructor c of
            Match (PVar v) exp -> do
              v' <- instantiateName v
              liftM (letE' [(v', varE e'Name)]) $ instantiate exp

            Match (PCon _ []) exp -> instantiate exp

            Match (PCon _ ps) exp -> do
              bindings' <- zipWithM mkBinding [0..] psNames
              exp'      <- instantiate exp
              return $ letE' bindings' exp'
              where 
                mkBinding i var = do 
                  var' <- instantiateName var 
                  return (var', eConstructorArg i)

                eConstructorArg i = appsE (TH.VarE 'constructorArgument) 
                                          [ intE i, intE j, varE e'Name ]

                psNames = map (\(PVar p) -> nUntyped p) ps
            
          -- Finds the corresponding match for constructor @c@
          matchFromConstructor c = 
            case find byMatch matches of
              Nothing -> case defaultMatch of
                            Nothing -> error $ "Algorithms.Eitherize.matchFromConstructor: no match for constructor '" ++ fromName c ++ "'"
                            Just m  -> m
              Just m  -> m

            where byMatch (Match (PVar _  ) _) = False
                  byMatch (Match (PCon p _) _) = untypedName p == c

          -- Finds the corresponding ADT for the matches
          getAdt = asks (find (any isConstructor . dAdtConstructors) . adts)
            where 
              PCon p _ = matchPattern $ head $ matches
              isConstructor (CCon c _) = untypedName p == c

      checkValidity e'Name = 
          TH.CondE (TH.AppE (TH.VarE 'isInvalid) (varE e'Name))
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

  instantiateUndefined = return $ TH.AppE (TH.VarE 'return) (TH.VarE 'undefined)

  instantiateBind (DBind (Binding name exp)) = do
    name'        <- instantiateName name
    exp'         <- instantiate exp
    profiledExp' <- is Profile >>= return . \case 
      False -> exp'
      True  -> case exp' of
        TH.LamE patterns exp'' -> TH.LamE patterns 
                                $ appsE (TH.VarE 'traced) [ stringE name, exp'' ]
        _                      -> appsE (TH.VarE 'traced) [ stringE name, exp' ]

    return [ instantiateSignature name' numArgs, valD' name' profiledExp' ]
    where
      numArgs = case exp of
        ELam ps _ -> length ps
        _         -> 0

instantiateSignature :: (Namelike n) => n -> Int -> TH.Dec
instantiateSignature name numArgs =
  let type_ = foldr (\l r -> TH.AppT (TH.AppT TH.ArrowT l) r) 
                    (TH.AppT (TH.ConT ''CO4) (TH.ConT ''EncodedAdt))
                    (replicate numArgs $ TH.ConT ''EncodedAdt)
  in
    sigD' name $ TH.ForallT [] [] type_

-- |@eitherize prof p@ eitherizes a first order program into a Template-Haskell program.
-- @prof@ enables profiling.
eitherize :: (MonadUnique u,MonadConfig u) => Program -> u [TH.Dec]
eitherize program = do
  typedProgram <- schemes program
  withPrelude  <- is ImportPrelude

  let (pAdts,pFuns)  = splitDeclarations typedProgram 
      adts           = if withPrelude then pAdts ++ preludeAdtDeclarations
                                      else pAdts
      pToplevelNames = map boundName $ programToplevelBindings program
      toplevelNames  = if withPrelude 
                       then pToplevelNames ++ unparsedNames
                       else pToplevelNames

  decls   <- execWriterT $ runAdtInstantiator $ collect adts
  values' <- liftM concat $ runReaderT 
                (runExpInstantiator $ instantiate $ map DBind pFuns)
                (ExpInstantiatorData toplevelNames adts)
                         
  return $ {-deleteSignatures $-} decls ++ values'


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
