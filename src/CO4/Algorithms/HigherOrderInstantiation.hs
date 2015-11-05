{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module CO4.Algorithms.HigherOrderInstantiation
  (hoInstantiation)
where

import           Control.Monad.Reader
import           Control.Monad.State.Strict hiding (State)
import qualified Data.Map as M
import           Data.List (nub,partition,(\\))
import           Data.Maybe (fromMaybe,mapMaybe)
import           CO4.Language
import           CO4.Util (programDeclarations,programFromDeclarations,removeSignature)
import           CO4.Unique (MonadUnique,newName,originalName)
import           CO4.Algorithms.Instantiator
import qualified CO4.Algorithms.HindleyMilner as HM
import           CO4.Algorithms.Free (free)
import           CO4.Algorithms.Util (eraseTypedNames,collapseApplications,collapseAbstractions)
import           CO4.TypesUtil
import           CO4.Names
import           CO4.Config (MonadConfig,Config(ImportPrelude),is,instantiationDepth,fromConfigs)
import           CO4.Prelude (unparsedNames)

data Env = Env { -- |Bindings of higher order expressions
                 hoBindings      :: M.Map Name Expression 
                 -- |Not instantiable declarations 
               , notInstantiable :: [Declaration]
                 -- |Current instantiation depth
               , depth           :: Int
               }

type CacheKey   = (Name,[Expression])
type CacheValue = Name 
type Cache      = M.Map CacheKey CacheValue

data State = State { cache     :: Cache
                   , instances :: [Binding]
                   }

newtype Instantiator u a = Instantiator 
  { runInstantiator :: ReaderT Env (StateT State u) a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadState State, MonadUnique, MonadConfig)

writeInstance :: MonadUnique u => Binding -> Instantiator u ()
writeInstance instance_ = 
  modify (\state -> state { instances = (instances state) ++ [instance_] })

writeCacheItem :: MonadUnique u => CacheKey -> CacheValue -> Instantiator u ()
writeCacheItem key value = 
  modify (\state -> state { cache = M.insert key value $ cache state })

hoBinding :: Monad u => Name -> Instantiator u (Maybe Expression)
hoBinding name = asks hoBindings >>= return . M.lookup name 

instance (MonadUnique u,MonadConfig u) => MonadInstantiator (Instantiator u) where

  instantiateVar (EVar name) = liftM (fromMaybe $ EVar name) $ hoBinding name
              
  instantiateApp (EApp (EVar f) args) = 
    hoBinding f >>= \case 
      Nothing -> return (EApp $ EVar f) `ap` instantiate args

      Just (ELam params e) -> do
        args' <- mapM instantiateExpression args

        let (  foParameters     
             , foArguments      
             , hoParameters     
             , hoArguments  )  = splitByOrder $ zip params args'

        freeInHoArguments  <- liftM (nub . concat) $ mapM freeNames hoArguments
            
        let instanceParameters = foParameters ++ freeInHoArguments 
            instanceArguments  = foArguments  ++ map EVar freeInHoArguments

        gets (M.lookup (f, hoArguments) . cache) >>= \case
          Nothing -> do
            instanceName <- newName $ fromName (originalName f) ++ "HOInstance"

            writeCacheItem (f, hoArguments) instanceName

            let newBindings = zip hoParameters hoArguments
                instanceRHS = ELam instanceParameters e

            instantiateExpInModifiedEnv f newBindings instanceRHS 
              >>= writeInstance . Binding instanceName 

            return $ EApp (EVar instanceName) instanceArguments

          Just instance_ -> return $ EApp (EVar instance_) instanceArguments   

      Just e -> instantiateApp $ EApp e args

  instantiateApp (EApp f args) = return EApp `ap` instantiate f `ap` instantiate args
  
splitByOrder :: [(Name,Expression)] -> ([Name],[Expression],[Name],[Expression])
splitByOrder =
  foldl (\(foParam,foArg,hoParam,hoArg) (parameter,argument) ->
            if hasFunType parameter 
            then (foParam,foArg,hoParam ++ [parameter],hoArg ++ [argument])
            else (foParam ++ [parameter],foArg ++ [argument],hoParam,hoArg)
  ) ([],[],[],[]) 

  where hasFunType (NTyped _ s) = isFunType $ typeOfScheme s

isInstantiable :: Declaration -> Bool
isInstantiable declaration = case declaration of
  DBind (Binding n _) -> isHigherOrder n
  _                   -> False

  where isHigherOrder (NTyped _ scheme) = 
          let paramTypes = argumentTypes $ typeOfScheme scheme
          in
            any isFunType paramTypes

-- |Program must not contain local abstractions
hoInstantiation :: (MonadConfig u,MonadUnique u) => Program -> u Program
hoInstantiation program = do
  maxDepth     <- fromConfigs instantiationDepth
  typedProgram <- HM.schemes program

  let (instantiable,rest) = partition isInstantiable $ programDeclarations typedProgram

      initBindings        = map (\(DBind (Binding n e)) -> (n,e)) instantiable
      initEnv             = Env (M.fromList initBindings) rest maxDepth
      initState           = State M.empty []
      instantiableNames   = map (untypedName . fst) initBindings

  (p',state) <- runStateT (runReaderT (runInstantiator $ instantiate rest) initEnv) 
                          initState
  return $ eraseTypedNames
         $ collapseApplications 
         $ collapseAbstractions
         $ flip (foldl $ flip removeSignature) instantiableNames
         $ programFromDeclarations
         $ p' ++ (map DBind $ instances state)
   
freeNames :: (MonadUnique u,MonadConfig u) => Expression -> Instantiator u [Name]
freeNames exp = 
  let frees                                 = free exp
      toplevelName (DBind (Binding name _)) = Just name
      toplevelName _                        = Nothing
  in do
    parsedToplevelNames <- liftM (mapMaybe toplevelName) $ asks notInstantiable
    toplevelNames       <- is ImportPrelude >>= \case 
      True  -> return $ parsedToplevelNames ++ unparsedNames
      False -> return $ parsedToplevelNames

    instanceNames <- liftM (map boundName) $ gets instances
    return $ frees \\ (toplevelNames ++ instanceNames)

instantiateExpInModifiedEnv :: (MonadUnique u, MonadConfig u, Namelike n) 
                              => n -> [(Name,Expression)] -> Expression 
                              -> Instantiator u Expression
instantiateExpInModifiedEnv fName newBindings =
  let modifyEnv env = 
        if depth env == 0 
        then error $ "Algorithms.Instantiation: reached maximum depth while instantiating '" ++ fromName fName ++ "'"
        else env { depth = depth env - 1
                 , hoBindings = M.union (M.fromList newBindings) (hoBindings env) }
  in
    local modifyEnv . instantiateExpression
