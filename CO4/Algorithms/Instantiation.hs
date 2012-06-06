{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.Instantiation
  (instantiation)
where

import           Control.Monad.Reader
import           Control.Monad.State hiding (State)
import           Control.Applicative ((<$>))
import qualified Data.Map as M
import           Data.List (nub,partition,(\\))
import           Data.Maybe (fromJust)
import           CO4.Language
import           CO4.Unique
import           CO4.Util (boundName)
import           CO4.Algorithms.Instantiator
import qualified CO4.Algorithms.HindleyMilner as HM
import           CO4.Names (typedName)
import           CO4.TypesUtil

data Env   = Env { bindings        :: M.Map Name Expression
                 , notInstantiable :: [Declaration]
                 , depth           :: Int
                 }

type CacheKey   = (Name,[Expression],[Type])
type CacheValue = Name 
type Cache      = M.Map CacheKey CacheValue

data State = State { cache     :: Cache
                   , instances :: [Declaration]
                   }

newtype Instantiator a = Instantiator (ReaderT Env (StateT State Unique) a)
  deriving ( Functor, Monad, MonadReader Env, MonadState State )

instance MonadInstantiator Instantiator where

  instantiateVar (EVar name) = 
    M.findWithDefault (EVar name) name <$> asks bindings

  instantiateTApp (ETApp (EVar fName@(TypedName fString fScheme)) typeApps) = do
    f' <- fromJust . M.lookup fName <$> asks bindings
    case f' of
      ETLam typeParams e -> do
        cache <- gets cache
        case M.lookup (fName, [], typeApps) cache of
          Nothing -> 
            let instantiatedE  = HM.substitutes (zip typeParams typeApps) e
                instanceScheme = HM.instantiateSchemeApp fScheme typeApps
            in do
              instanceName <- liftUnique $ do 
                                untyped <- newName' $ fString ++ "Instance"
                                return $ typedName untyped instanceScheme

              writeCacheItem (fName, [], typeApps) instanceName 
              instantiatedE' <- local (decreaseDepth fString)
                                  $ instantiateExpression instantiatedE
              writeInstance $ DBind instanceName instantiatedE'
              return $ EVar instanceName
              
      exp -> return exp

  instantiateApp (EApp (ETApp (EVar fName@(TypedName fString fScheme)) typeApps) args) = do
    f'    <- fromJust . M.lookup fName <$> asks bindings
    args' <- mapM instantiateExpression args

    case f' of
      ETLam typeParams (ELam parameters e) -> do

        let subst              = zip typeParams typeApps
            monoScheme         = HM.instantiateSchemeApp fScheme typeApps
            parameters'        = HM.substitutes subst parameters
            e'                 = HM.substitutes subst e
            (  foParameters     
             , foArguments      
             , hoParameters     
             , hoArguments  )  = splitFirstHigherOrder parameters' args'
            curriedParameters  = drop (length args') $ parameters'

        freeInHoArguments  <- (nub . concat) <$> mapM freeNames hoArguments
            
        let allFoParameters = foParameters ++ freeInHoArguments ++ curriedParameters
            allFoArguments  = foArguments  ++ map EVar freeInHoArguments
            
            instanceScheme  = 
              let argsT   = map (\(TypedName _ s) -> fromSType s) allFoParameters
                  resultT = resultType $ fromSType monoScheme
              in
                HM.generalizeAll $ functionType argsT resultT

        cache <- gets cache
        case M.lookup (fName, hoArguments, typeApps) cache of
          Nothing -> do
            instanceName <- liftUnique $ do 
                              untyped <- newName' $ fString ++ "Instance"
                              return $ typedName untyped instanceScheme

            writeCacheItem (fName, hoArguments, typeApps) instanceName

            let modifyEnv      = addBindings (zip hoParameters hoArguments)
                               . decreaseDepth fString
                instanceRHS    = ELam allFoParameters e'

            e'' <- local modifyEnv $ instantiateExpression instanceRHS 
            writeInstance $ DBind instanceName e''
            return $ EApp (EVar instanceName) allFoArguments

          Just instance_ -> return $ EApp (EVar instance_) allFoArguments   

      _ -> return $ EApp f' args'

  instantiateApp (EApp f args) = do 
    f'    <- instantiateExpression f
    args' <- mapM instantiateExpression args
    return $ EApp f' args'
  
splitFirstHigherOrder :: [Name] -> [Expression] -> ([Name],[Expression],[Name],[Expression])
splitFirstHigherOrder parameters arguments =
  foldl (\(foParam,foArg,hoParam,hoArg) (parameter,argument) ->
            if hasFunType parameter 
            then (foParam,foArg,hoParam ++ [parameter],hoArg ++ [argument])
            else (foParam ++ [parameter],foArg ++ [argument],hoParam,hoArg)
  ) ([],[],[],[]) $ zip parameters arguments

hasFunType :: Name -> Bool
hasFunType (TypedName _ s) = isFunType $ typeOfScheme s

isPolymorphic :: Name -> Bool
isPolymorphic (TypedName _ (SForall {})) = True
isPolymorphic (TypedName _ (SType _))   = False

isHigherOrder :: Name -> Bool
isHigherOrder (TypedName _ scheme) = 
  let paramTypes = argumentTypes $ typeOfScheme scheme
  in
    any isFunType paramTypes

isInstantiable :: Name -> Bool
isInstantiable name = (isHigherOrder name) || (isPolymorphic name)

liftUnique :: Unique a -> Instantiator a
liftUnique = Instantiator . lift . lift

writeInstance :: Declaration -> Instantiator ()
writeInstance declaration = 
  modify (\state -> state { instances = (instances state) ++ [declaration] })

writeCacheItem :: CacheKey -> CacheValue -> Instantiator ()
writeCacheItem key value = 
  modify (\state -> state { cache = M.insert key value $ cache state })

instantiation :: Int -> Program -> Unique Program
instantiation maxDepth p = do
  let (instantiable,rest) = partition (\(DBind n _) -> isInstantiable n) p 

      initBindings      = M.fromList $ map (\(DBind n e) -> (n,e)) instantiable
      initEnv           = Env initBindings rest maxDepth
      initState         = State M.empty []
      Instantiator inst = instantiateProgram rest

  (p',state) <- runStateT (runReaderT inst initEnv) initState
  return $ p' ++ (instances state)
   
freeNames :: Expression -> Instantiator [Name]
freeNames exp = 
  let freeInPrelude = HM.freeInPrelude exp
  in do
    topLevelBounds <- map boundName <$> asks notInstantiable
    instanceNames  <- map boundName <$> gets instances
    return $ freeInPrelude \\ (topLevelBounds ++ instanceNames)

addBindings :: [(Name,Expression)] -> Env -> Env
addBindings bindings' env = 
  env { bindings = M.union (M.fromList bindings') (bindings env) }

decreaseDepth :: String -> Env -> Env
decreaseDepth fName env@(Env { depth = d }) = 
  if d == 0
  then error $ "Instantiation: reached maximum depth while instantiating '" ++ fName ++ "'"
  else env { depth = d - 1 }
  
