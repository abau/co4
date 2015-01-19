{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# language LambdaCase #-}

module CO4.CodeGen
  (codeGen, codeGenAdt)
where

import           Prelude hiding (undefined)
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Applicative (Applicative)
import           Data.List (find)
import qualified Language.Haskell.TH as TH
import           CO4.Language
import           CO4.Util
import           CO4.THUtil
import           CO4.Names 
import           CO4.Algorithms.THInstantiator hiding (instantiateSignature)
import           CO4.Algorithms.Collector
import           CO4.Unique
import           CO4.CodeGen.Names
import           CO4.CodeGen.DecodeInstance (decodeInstance)
import           CO4.CodeGen.EncodeableInstance (encodeableInstance)
import           CO4.CodeGen.TypedAllocator (allocators)
import           CO4.EncodedAdt 
  (EncodedAdt,encUndefined,encodedConstructor,onValidDiscriminant,ifReachable,caseOf,constructorArgument)
import           CO4.Monad (CO4,withCallCache,traced,profiledCase)
import           CO4.Config (MonadConfig,is,Config(..))
import           CO4.Prelude (preludeAdtDeclarations,unparsedNames) 
import           CO4.PPrint

newtype AdtInstantiator u a = AdtInstantiator 
  { runAdtInstantiator :: WriterT [TH.Dec] u a } 
  deriving (Functor, Applicative, Monad, MonadConfig, MonadWriter [TH.Dec], MonadUnique)

instance (MonadUnique u,MonadConfig u) => MonadCollector (AdtInstantiator u) where

  collectAdt adt = do
    is OnlyAllocators >>= \case
      False -> do
        zipWithM_ mkEncodedConstructor [0..] $ adtConstructors adt

        decodeInstance adt     >>= tellOne
        encodeableInstance adt >>= tellOne

        is NoAllocators >>= \case
          True  -> return ()
          False -> allocators adt >>= tell

      True -> allocators adt >>= tell
    where 
      mkEncodedConstructor i (CCon name args) = do
        paramNames <- forM args $ const $ newName ""

        let exp = appsE (TH.VarE 'encodedConstructor) 
                      [ intE i
                      , intE $ length $ adtConstructors adt
                      , TH.ListE $ map varE paramNames ]
        tellOne $ valD' (encodedConsName name) 
                $ if null args 
                  then exp
                  else lamE' paramNames exp

      tellOne x    = tell [x]

type ToplevelName = Name
data ExpInstantiatorData = ExpInstantiatorData 
  { toplevelNames :: [ToplevelName]
  , adts          :: [Adt]
  }

newtype ExpInstantiator u a = ExpInstantiator 
  { runExpInstantiator :: ReaderT ExpInstantiatorData u a } 
  deriving (Functor, Applicative, Monad, MonadUnique, MonadConfig, MonadReader ExpInstantiatorData)

isToplevelName :: Monad u => ToplevelName -> ExpInstantiator u Bool
isToplevelName name = asks $ elem name . toplevelNames

instance (MonadUnique u,MonadConfig u) => MonadTHInstantiator (ExpInstantiator u) where

  instantiateName name = encodedName name >>= return . convertName

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
          n | n == natName -> case args of
            [ECon w,ECon i] -> return $ appsE (varE fName') [nameToIntE w,nameToIntE i]
            _               -> error $ "CodeGen.instantiateApp: nat"

          n | n == "assertKnownLoc" -> case args of
            [ECon l,ECon c,_] -> bindAndApplyArgs 
                                   (\[e'] -> appsE (varE fName') [nameToIntE l,nameToIntE c,e'])
                                   $ drop 2 args'
            _                 -> error $ "CodeGen.instantiateApp: assertKnownLoc"

          _ | cache  -> bindAndApplyArgs (\args'' -> 
                          appsE (TH.VarE 'withCallCache) 
                          [ TH.TupE [ stringE fName', TH.ListE args'']
                          , appsE (varE fName') args''
                          ]) args'
          _         -> bindAndApplyArgs (appsE $ varE fName') args'

  instantiateCase (ECase e ms) = 
    getAdt >>= \case 
      Nothing  -> error "CodeGen.instantiateCase: no ADT found"
      Just adt -> do
        let numCons = length $ adtConstructors adt

        e'Name <- newName "bindCase"
        e'     <- case e of 
            EApp (EVar f) [ECon l, ECon c, d] | fromName f == "markedDiscriminant" -> do
              d' <- instantiate d
              bindAndApplyArgs 
                (\[dName] -> appsE (TH.VarE 'profiledCase) [ nameToIntE l
                                                           , nameToIntE c
                                                           , intE numCons
                                                           , dName]) [d']
            _ -> instantiate e

        ms' <- instantiateMatches e'Name adt

        let e'Binding = bindS' e'Name e'

        if numCons == 1
          then return $ TH.DoE [ e'Binding, TH.NoBindS $ checkValidity e'Name numCons
                                                       $ head ms' ]
          else do 
            caseOfE <- bindAndApply 
                         (\ms'Names -> [ varE e'Name, TH.ListE $ map varE ms'Names ])
                         (appsE $ TH.VarE 'caseOf) ms'

            return $ TH.DoE [ e'Binding, TH.NoBindS $ checkValidity e'Name numCons
                                                    $ caseOfE ]
    where 
      -- Instantiate matches
      instantiateMatches e'Name adt = forM (zip [0..] $ adtConstructors adt) $ \(i,cons) ->
        do match' <- instantiateMatch i cons
           return $ appsE (TH.VarE 'ifReachable) [varE e'Name, intE i, intE numCons, match']
        where
          numCons = length $ adtConstructors adt

          -- Default match
          defaultMatch = case last ms of m@(Match (PVar _) _) -> Just m
                                         _                    -> Nothing

          -- Instantiate match of @j@-th constructor, namely @CCon c _@
          instantiateMatch j (CCon c _) = case matchFromConstructor c of
            Match (PVar v) exp -> do
              v' <- instantiateName v
              liftM (letE' [(v', varE e'Name)]) $ instantiate exp

            Match (PCon _ []) exp -> instantiate exp

            Match (PCon _ ps) exp -> do
              bindings' <- zipWithM mkBinding [0..] $ map toPsName ps
              exp'      <- instantiate exp
              return $ letE' bindings' exp'
              where 
                mkBinding i var = do 
                  var' <- instantiateName var 
                  return (var', eConstructorArg i)

                eConstructorArg i = appsE (TH.VarE 'constructorArgument) 
                                          [ intE numCons, intE i, intE j, varE e'Name ]

                toPsName (PVar p) = nUntyped p
                toPsName x        = error $ "CodeGen.instantiateMatch.toPsName: invalid nested pattern (" ++ show (pprint x) ++ ")"

          -- Finds the corresponding match for constructor @c@
          matchFromConstructor c = 
            case find byMatch ms of
              Nothing -> case defaultMatch of
                            Nothing -> error $ "CodeGen.matchFromConstructor: no match for constructor '" ++ fromName c ++ "'"
                            Just m  -> m
              Just m  -> m

            where byMatch (Match (PVar _  ) _) = False
                  byMatch (Match (PCon p _) _) = untypedName p == c

      -- Finds the corresponding ADT for the matches
      getAdt = asks (find (any isConstructor . adtConstructors) . adts)
        where 
          PCon p _ = matchPattern $ head $ ms
          isConstructor (CCon c _) = untypedName p == c

      checkValidity e'Name numCons caseOfE = 
        appsE (TH.VarE 'onValidDiscriminant) [varE e'Name, intE numCons, caseOfE]

  instantiateLet (ELet bindings exp) = do
    exp'      <- instantiate exp 
    bindings' <- mapM bindValue bindings
    return $ TH.DoE $ bindings' ++ [ TH.NoBindS exp' ]

    where 
      bindValue (Binding name value) = do
        name'  <- instantiateName name
        value' <- instantiate value
        return $ bindS' name' value'

  instantiateUndefined = return $ TH.AppE (TH.VarE 'return) (TH.VarE 'encUndefined)

  instantiateBind (DBind (Binding name exp)) = do
    name'        <- instantiateName name
    exp'         <- instantiate exp
    profiledExp' <- is Profile >>= return . \case 
      False -> exp'
      True  -> case exp' of
        TH.LamE patterns exp'' -> TH.LamE patterns 
                                $ appsE (TH.VarE 'traced) [ stringE name, exp'' ]
        _                      -> appsE (TH.VarE 'traced) [ stringE name, exp' ]

    return $ valD' name' profiledExp'

instantiateSignature :: MonadConfig u => Binding -> u TH.Dec
instantiateSignature (Binding name expression) = do
  name' <- encodedName name >>= return . convertName
  return $ TH.SigD name' $ TH.ForallT [] [] type_
  where
    numArgs = case expression of
      ELam xs _ -> length xs
      _         -> 0

    type_ = foldr (\l r -> TH.AppT (TH.AppT TH.ArrowT l) r) 
                  (TH.AppT (TH.ConT ''CO4) (TH.ConT ''EncodedAdt))
                  (replicate numArgs $ TH.ConT ''EncodedAdt)

-- |@codeGen p@ transforms a co4 program into a Template-Haskell program.
-- @p@ must be first-order and fully instantiated.
codeGen :: (MonadUnique u,MonadConfig u) => Program -> u [TH.Dec]
codeGen program = do
  withPrelude  <- is ImportPrelude

  let (pAdts,pValues,_) = splitDeclarations program
      adts           = if withPrelude then pAdts ++ preludeAdtDeclarations
                                      else pAdts
      pToplevelNames = map boundName $ programToplevelBindings program
      toplevelNames  = if withPrelude 
                       then pToplevelNames ++ unparsedNames
                       else pToplevelNames

  adts'   <- execWriterT $ runAdtInstantiator $ collect adts

  values' <- runReaderT (runExpInstantiator $ instantiate $ map DBind pValues)
                        (ExpInstantiatorData toplevelNames adts)

  sigs'   <- mapM instantiateSignature pValues
                         
  return $ adts' ++ values' ++ sigs'

-- |@codeGenAdt p@ only runs ADT related code generators.
codeGenAdt :: (MonadUnique u,MonadConfig u) => Program -> u [TH.Dec]
codeGenAdt program = do
  withPrelude  <- is ImportPrelude

  let (pAdts,_,_) = splitDeclarations program
      adts        = if withPrelude then pAdts ++ preludeAdtDeclarations
                                   else pAdts

  execWriterT $ runAdtInstantiator $ collect adts

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
