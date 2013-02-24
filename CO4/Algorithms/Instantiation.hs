{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.Instantiation
  (instantiation)
where

import           Control.Monad.Reader
import           Control.Monad.State hiding (State)
import qualified Data.Map as M
import           Data.List (nub,partition,(\\))
import           Data.Maybe (fromJust,fromMaybe,mapMaybe)
import           CO4.Language
import           CO4.Util (nTyped,programDeclarations,programFromDeclarations,mainName)
import           CO4.Unique (MonadUnique,newName)
import           CO4.Algorithms.Instantiator
import qualified CO4.Algorithms.HindleyMilner as HM
import           CO4.Algorithms.Free (free)
import           CO4.Algorithms.Util (eraseTypedNames,eraseTLamTApp,collapseApplications)
import           CO4.TypesUtil
import           CO4.Names

data Env = Env { -- |Bindings of higher order expressions
                 hoBindings      :: M.Map Name Expression 
                 -- |Not instantiable declarations 
               , notInstantiable :: [Declaration]
                 -- |Current instantiation depth
               , depth           :: Int
               }

type CacheKey   = (Name,[Expression],[Type])
type CacheValue = Name 
type Cache      = M.Map CacheKey CacheValue

data State = State { cache     :: Cache
                   , instances :: [Binding]
                   }

newtype Instantiator u a = Instantiator 
  { runInstantiator :: ReaderT Env (StateT State u) a }
  deriving (Monad, MonadReader Env, MonadState State, MonadUnique )

writeInstance :: MonadUnique u => Binding -> Instantiator u ()
writeInstance instance_ = 
  modify (\state -> state { instances = (instances state) ++ [instance_] })

writeCacheItem :: MonadUnique u => CacheKey -> CacheValue -> Instantiator u ()
writeCacheItem key value = 
  modify (\state -> state { cache = M.insert key value $ cache state })

hoBinding :: Monad u => Name -> Instantiator u (Maybe Expression)
hoBinding name = asks hoBindings >>= return . M.lookup name 

unsafeHoBinding :: Monad u => Name -> Instantiator u Expression
unsafeHoBinding name = hoBinding name >>= return . fromJust 

newTypedInstanceName :: (MonadUnique u, Namelike n) => n -> Scheme -> u Name
newTypedInstanceName originalName scheme = do
  untyped <- newName $ fromName originalName ++ "Instance"
  return $ nTyped untyped scheme

instance MonadUnique u => MonadInstantiator (Instantiator u) where

  instantiateVar (EVar name) = liftM (fromMaybe $ EVar name) $ hoBinding name

  -- Instantiate polymorphic constants
  instantiateTApp (ETApp (EVar f) typeApps) = do
    ETLam typeParams e <- unsafeHoBinding f
    cache <- gets cache
    case M.lookup (f, [], typeApps) cache of
      Nothing -> 
        let instanceRHS    = HM.substitutes (zip typeParams typeApps) e
            instanceScheme = HM.instantiateSchemeApp (schemeOfName f) typeApps
        in do
          instanceName <- newTypedInstanceName f instanceScheme

          writeCacheItem (f, [], typeApps) instanceName 

          instantiateExpInModifiedEnv f [] instanceRHS
            >>= writeInstance . Binding instanceName 

          return $ EVar instanceName
      Just instance_ -> return $ EVar instance_
              
  -- Instantiate polymorphic and/or higher order functions
  instantiateApp (EApp (ETApp (EVar f) typeApps) args) = do
    
    ETLam typeParams (ELam params e) <- unsafeHoBinding f
    args'                            <- mapM instantiateExpression args

    let substitution       = zip typeParams typeApps
        monomorphicScheme  = HM.instantiateSchemeApp (schemeOfName f) typeApps
        params'            = HM.substitutes substitution params
        (  foParameters     
         , foArguments      
         , hoParameters     
         , hoArguments  )  = splitByOrder $ zip params' args'

    freeInHoArguments  <- liftM (nub . concat) $ mapM freeNames hoArguments
        
    let instanceParameters = foParameters ++ freeInHoArguments 
        instanceArguments  = foArguments  ++ map EVar freeInHoArguments
        
        instanceScheme =
          let argTypes    = map (fromSType . schemeOfName) instanceParameters
              resultType_ = resultType $ fromSType monomorphicScheme
          in 
            HM.generalizeAll $ functionType argTypes resultType_

    cache <- gets cache
    case M.lookup (f, hoArguments, typeApps) cache of
      Nothing -> do
        instanceName <- newTypedInstanceName f instanceScheme 

        writeCacheItem (f, hoArguments, typeApps) instanceName

        let newBindings = zip hoParameters hoArguments
            instanceRHS = ELam instanceParameters $ HM.substitutes substitution e

        instantiateExpInModifiedEnv f newBindings instanceRHS 
          >>= writeInstance . Binding instanceName 

        return $ EApp (EVar instanceName) instanceArguments

      Just instance_ -> return $ EApp (EVar instance_) instanceArguments   

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
  DBind (Binding n _) -> (isHigherOrder n) || (isPolymorphic n)
  _                   -> False

  where isPolymorphic (NTyped _ (SForall {})) = True
        isPolymorphic (NTyped _ (SType _))    = False

        isHigherOrder (NTyped _ scheme) = 
          let paramTypes = argumentTypes $ typeOfScheme scheme
          in
            any isFunType paramTypes

instantiation :: MonadUnique u => Int -> Program -> u Program
instantiation maxDepth program = do
  typedProgram <- HM.schemesConfig (HM.HMConfig HM.IntroduceVarTLamTApp True) 
                                   HM.emptyContext program

  let (instantiable,rest) = partition isInstantiable $ programDeclarations typedProgram

      initBindings        = M.fromList $ map (\(DBind (Binding n e)) -> (n,e)) instantiable
      initEnv             = Env initBindings rest maxDepth
      initState           = State M.empty []

  (p',state) <- runStateT (runReaderT (runInstantiator $ instantiate rest) initEnv) 
                          initState
  return $ eraseTLamTApp
         $ eraseTypedNames
         $ collapseApplications -- TODO: really?
         $ programFromDeclarations (mainName program) 
         $ p' ++ (map DBind $ instances state)
   
freeNames :: MonadUnique u => Expression -> Instantiator u [Name]
freeNames exp = 
  let frees                                 = free exp
      toplevelName (DBind (Binding name _)) = Just name
      toplevelName _                        = Nothing
  in do
    toplevelNames <- liftM (mapMaybe toplevelName) $ asks notInstantiable
    instanceNames <- liftM (map boundName        ) $ gets instances
    return $ frees \\ (toplevelNames ++ instanceNames)

instantiateExpInModifiedEnv :: (MonadUnique u, Namelike n) 
                              => n -> [(Name,Expression)] -> Expression 
                              -> Instantiator u Expression
instantiateExpInModifiedEnv fName newBindings =
  let modifyEnv env = 
        if depth env == 0 then error $ "Instantiation: reached maximum depth while instantiating '" ++ fromName fName ++ "'"
        else env { depth = depth env - 1
                 , hoBindings = M.union (M.fromList newBindings) (hoBindings env) }
  in
    local modifyEnv . instantiateExpression
