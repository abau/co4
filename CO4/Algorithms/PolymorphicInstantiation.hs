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
import           CO4.Util (programFromDeclarations,programDeclarations)
import           CO4.TypesUtil (quantifiedNames,schemeOfName)
import           CO4.Names (fromName)
import           CO4.Prelude (unparsedNames)

polyInstantiation :: (MonadConfig u, MonadUnique u) => Program -> u Program 
polyInstantiation program = do
  typedProgram <- initialContext >>= \c -> schemesConfig (HMConfig IntroduceVarTLamTApp True)
                                                         c program
  --return typedProgram
  let (instantiable,rest) = partition isInstantiable $ programDeclarations typedProgram

  (p',state) <- runStateT (runReaderT (runInstantiator $ instantiate rest) 
                                      (initEnv instantiable)
                          ) emptyState

  return $ programFromDeclarations 
         $ p' ++ (map DBind $ instances state)

isInstantiable :: Declaration -> Bool
isInstantiable declaration = case declaration of
  DBind (Binding n (ETLam {})) -> assert (isPolymorphic n      ) True
  DBind (Binding n _         ) -> assert (not $ isPolymorphic n) False
  _                            -> False
  where 
    isPolymorphic (NTyped _ (SType   {})) = False
    isPolymorphic (NTyped _ (SForall {})) = True

type CacheKey   = (Name,[Type]) -- (polymorphic name, type arguments)
type CacheValue = Name          --  monomorphic name
type Cache      = M.Map CacheKey CacheValue

data State = State { cache     :: Cache
                   , instances :: [Binding]
                   }
emptyState = State M.empty []

writeInstance :: MonadUnique u => Binding -> Instantiator u ()
writeInstance instance_ = 
  modify (\state -> state { instances = (instances state) ++ [instance_] })

writeCacheItem :: MonadUnique u => CacheKey -> CacheValue -> Instantiator u ()
writeCacheItem key value = 
  modify (\state -> state { cache = M.insert key value $ cache state })

data Env = Env { boundTypeVars :: M.Map UntypedName Type
               , polyBindings  :: M.Map Name Expression 
               }

initEnv = Env M.empty 
        . M.fromList 
        . map (\(DBind (Binding n e)) -> (n,e))

bindTypeVars :: [(UntypedName,Type)] -> Env -> Env
bindTypeVars bs env = env { boundTypeVars = M.union (M.fromList bs) $ boundTypeVars env }

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
  
  instantiateTApp (ETApp (EVar f) types) = do
    types' <- mapM instantiateType types
    gets (M.lookup (f,types') . cache) >>= \case
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

          writeCacheItem (f, types) $ instanceName

          asks (M.lookup f . polyBindings) >>= \case 
            Nothing -> error $ "Algorithms.PolymorphicInstantiation.makeInstance: '" ++ fromName f ++ "' not found"
            Just (ETLam typeVars' body) -> assert (typeVars == typeVars') $ do
              body' <- instantiate body
              writeInstance $ Binding instanceName body'
              return instanceName
          where
            typeVars     = quantifiedNames $ schemeOfName f
            newBoundVars = zip typeVars types
