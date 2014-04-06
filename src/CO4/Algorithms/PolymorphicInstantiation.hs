{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module CO4.Algorithms.PolymorphicInstantiation
  (polyInstantiation)
where

import           Control.Exception (assert)
import           Control.Monad.State.Strict hiding (State)
import           Control.Monad.Reader
import qualified Data.Map as M
import           Data.List (partition)
import           CO4.Language
import           CO4.Unique
import           CO4.Config (MonadConfig,is,Config(ImportPrelude))
import           CO4.Algorithms.HindleyMilner (HMConfig(..),IntroduceTLamTApp(..))
import           CO4.Algorithms.HindleyMilner (schemesConfig,initialContext)
import           CO4.Algorithms.Instantiator
import           CO4.Util (programFromDeclarations,programDeclarations,removeSignature)
import           CO4.TypesUtil (quantifiedNames,schemeOfName)
import           CO4.Names (fromName,untypedName)
import           CO4.Prelude (unparsedNames)

polyInstantiation :: (MonadConfig u, MonadUnique u) => Program -> u Program 
polyInstantiation program = do
  typedProgram <- initialContext >>= \c -> schemesConfig (HMConfig IntroduceAllTLamTApp True)
                                                         c program
  let (instantiable,rest) = partition isInstantiableDecl $ programDeclarations typedProgram
      env                 = initEnv instantiable
      instantiableNames   = map (untypedName . fst) $ M.toList $ polyBindings env

  (p',state) <- runStateT (runReaderT (runInstantiator $ instantiate rest) env) emptyState

  let instances = map DBind $ getInstances (map (\(DBind b) -> boundName b) instantiable) state

  return $ flip (foldl $ flip removeSignature) instantiableNames
         $ programFromDeclarations
         $ p' ++ instances

isInstantiableDecl :: Declaration -> Bool
isInstantiableDecl declaration = case declaration of
  DBind binding -> isInstantiableBinding binding
  _             -> False

isInstantiableBinding :: Binding -> Bool
isInstantiableBinding binding = case binding of
  Binding n (ETLam {}) -> assert (isPolymorphic n      ) True
  Binding n _          -> assert (not $ isPolymorphic n) False
  where 
    isPolymorphic (NTyped _ (SType   {})) = False
    isPolymorphic (NTyped _ (SForall {})) = True

data State = State { 
    instanceNames :: M.Map Name (M.Map [Type] Name) -- name -> (types -> instance name)
  , instances     :: M.Map Name Expression          -- instance name -> instance body
  }
emptyState = State M.empty M.empty

writeInstanceName :: Name -> [Type] -> Name -> State -> State
writeInstanceName polyName args monoName state = state { instanceNames = go $ instanceNames state }
  where
    go = M.insertWith M.union polyName $ M.singleton args monoName

lookupInstanceNames :: Name -> State -> [Name]
lookupInstanceNames name state = case M.lookup name (instanceNames state) of
                                   Nothing -> []
                                   Just is -> M.elems is

lookupInstanceName :: Name -> [Type] -> State -> Maybe Name
lookupInstanceName name args state = M.lookup name (instanceNames state) 
                                 >>= M.lookup args

getInstances :: [Name] -> State -> [Binding]
getInstances names state = concatMap getInstances names
  where
    getInstances n = flip map (lookupInstanceNames n state) $ \n' -> 
      case M.lookup n' (instances state) of
        Nothing -> error $ "Algorithms.PolymorphicInstantiation.getInstances: '" ++ fromName n' ++ "' not found"
        Just b  -> Binding n' b

removeInstances :: [Name] -> State -> State
removeInstances = flip $ foldr $ \n -> removeInstanceNames' n . removeInstances' n
  where
    removeInstances' name state = 
      state { instances = foldr M.delete (instances state) $ lookupInstanceNames name state }

    removeInstanceNames' name state = 
      state { instanceNames = M.delete name $ instanceNames state }

writeInstance :: Name -> Expression -> State -> State
writeInstance name exp state = state { instances = M.insert name exp $ instances state }

data Env = Env { boundTypeVars :: M.Map UntypedName Type
               , polyBindings  :: M.Map Name Expression 
               }

initEnv = Env M.empty 
        . M.fromList 
        . map (\(DBind (Binding n e)) -> (n,e))

bindTypeVars :: [(UntypedName,Type)] -> Env -> Env
bindTypeVars bs env = env { boundTypeVars = M.union (M.fromList bs) $ boundTypeVars env }

addPolyBindings :: [Binding] -> Env -> Env
addPolyBindings bindings env = env { polyBindings = M.union (M.fromList bs) $ polyBindings env }
  where
    bs = map (\(Binding n e) -> (n,e)) bindings

newtype Instantiator u a = Instantiator { runInstantiator :: ReaderT Env (StateT State u) a }
  deriving (Monad, MonadReader Env, MonadState State, MonadUnique, MonadConfig)

instance (MonadUnique u,MonadConfig u) => MonadInstantiator (Instantiator u) where

  instantiateType = \case
    TVar v -> asks (M.lookup v . boundTypeVars) >>= \case
      Nothing -> return $ TVar v
      Just t  -> instantiateType t
    TCon c ts -> mapM instantiateType ts >>= return . TCon c

  instantiateScheme = \case 
    SType t     -> instantiateType t >>= return . SType
    SForall n s -> asks (M.lookup n . boundTypeVars) >>= \case
      Nothing -> instantiateScheme s >>= return . SForall n
      Just _  -> instantiateScheme s
  
  instantiateTApp (ETApp (ECon c) types) = assert (length types == length typeVars) $ do
    types' <- mapM instantiateType types
    local (bindTypeVars $ zip typeVars types') $ do 
      instanceScheme <- instantiate $ schemeOfName c
      return $ ECon $ NTyped (fromName c) instanceScheme
    where
      typeVars = quantifiedNames $ schemeOfName c

  instantiateTApp (ETApp (EVar f) types) = do
    types' <- mapM instantiateType types
    gets (lookupInstanceName f types') >>= \case
      Just f' -> return $ EVar f'
      Nothing -> 
        is ImportPrelude >>= \case
          True | f `elem` unparsedNames -> return $ EVar f
          _                             -> makeInstance f types' >>= return . EVar
    where
      makeInstance f types = assert (length types == length typeVars) $ 
        local (bindTypeVars newBoundVars) $ do 
          untypedInstanceName <- newNamelike $ fromName (originalName f) ++ "PolyInstance"
          instanceScheme      <- instantiate $ schemeOfName f

          let instanceName = NTyped untypedInstanceName instanceScheme

          modify $ writeInstanceName f types instanceName

          asks (M.lookup f . polyBindings) >>= \case 
            Nothing -> error $ "Algorithms.PolymorphicInstantiation.makeInstance: '" ++ fromName f ++ "' not found"
            Just (ETLam typeVars' body) -> assert (typeVars == typeVars') $ do
              body' <- instantiate body
              modify $ writeInstance instanceName body'
              return instanceName
          where
            typeVars     = quantifiedNames $ schemeOfName f
            newBoundVars = zip typeVars types

  instantiateLet (ELet bindings exp) =
    local (addPolyBindings instantiable) $ do
      rest' <- instantiate rest
      exp'  <- instantiate exp

      instances <- gets $ getInstances instantiableNames

      modify $ removeInstances instantiableNames
      return $ ELet (rest' ++ instances) exp'

    where
      (instantiable, rest) = partition isInstantiableBinding bindings
      instantiableNames    = map boundName instantiable
