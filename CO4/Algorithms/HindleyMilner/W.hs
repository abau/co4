{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.HindleyMilner.W
  ( HMConfig (..), IntroduceTLamTApp (..) 
  , schemeOfExp, schemesConfig, schemes, withSchemes)
where
  
import           Prelude hiding (lookup)
import           Control.Monad.Identity
import           Control.Monad.Reader
import qualified Data.List as L
import           Data.Maybe (fromJust)
import           CO4.Util 
import           CO4.Language
import           CO4.Unique (MonadUnique,newName)
import           CO4.Names
import           CO4.TypesUtil
import           CO4.Algorithms.HindleyMilner.Util
import           CO4.Algorithms.Bound (boundInScheme)
import           CO4.PPrint
import           CO4.Algorithms.Instantiator hiding (instantiate)
import           CO4.Algorithms.Util (introduceTypedNames,eraseTypedNames)
import           CO4.Algorithms.TopologicalSort (bindingGroups)

-- |Options whether and how to introduce type applications and type abstractions
data IntroduceTLamTApp = IntroduceAllTLamTApp  -- ^For all polymorphic expressions
                       | IntroduceVarTLamTApp  -- ^For polymorphic variables
                       | IntroduceConTLamTApp  -- ^For polymorphic constructors
                       | DontIntroduceTLamTApp -- ^Don't introduce any type applications or type abstractions

data HMConfig     = HMConfig { introduceTLamTApp      :: IntroduceTLamTApp 
                             , errorOnPolymorphicMain :: Bool
                             }
type HM u a       = ReaderT HMConfig u a
type BindingGroup = [Binding]

introduceVarTLamTApp :: Monad u => HM u Bool
introduceVarTLamTApp = do
  intro <- asks introduceTLamTApp
  return $ case intro of
    IntroduceAllTLamTApp -> True
    IntroduceVarTLamTApp -> True
    _                    -> False

introduceConTLamTApp :: Monad u => HM u Bool
introduceConTLamTApp = do
  intro <- asks introduceTLamTApp
  return $ case intro of
    IntroduceAllTLamTApp -> True
    IntroduceConTLamTApp -> True
    _                    -> False

runHm :: MonadUnique u => HMConfig -> HM u a -> u a
runHm = flip runReaderT

-- |Infers the scheme of an expression without introducing type abstractions
-- and type applications
schemeOfExp :: MonadUnique u => Expression -> u Scheme
schemeOfExp exp = do
  (subst,t,_) <- runHm (HMConfig DontIntroduceTLamTApp False) $ w context exp
  let t' = generalize (substitutes subst context) t
  return t'

  where context = emptyContext

-- |Annotates the scheme to all named expressions/declarations 
schemes :: MonadUnique u => Program -> u Program
schemes = schemesConfig (HMConfig DontIntroduceTLamTApp True) emptyContext

-- |@withSchemes f p@ applies @f@ to @p'@, where @p'@ equals @p@ after type inference.
-- Returns the result of @f p'@ with erased type information.
withSchemes :: MonadUnique u => (Program -> u Program) -> Program -> u Program
withSchemes f program = schemes program >>= f >>= return . eraseTypedNames

schemesConfig :: MonadUnique u => HMConfig -> Context -> Program -> u Program
schemesConfig hmConfig context program = do
  program' <- runHm hmConfig $ wProgram context program

  let schemeOfMain = schemeOfName $ boundName $ pMain program'

  if (null $ boundInScheme schemeOfMain) || (not $ errorOnPolymorphicMain hmConfig)
    then return $ runIdentity $ runTypeApp $ instantiateProgram program'
    else error $ "Algorithms.HindleyMilner: main must not have the polymorphic type '" ++ show (pprint schemeOfMain) ++ "'"

-- | While introducing explicit type applications, we apply an instantiated
-- type @t@ to polymorphic function symbols in the first place:
-- @map not xs@ becomes @map <(Bool -> Bool) -> [Bool] -> [Bool]> not xs@
-- @TypeApplicator@ unifies @t@ with the general type of @map@.
-- The final type application consists of the substituted variables:
-- @map <Bool,Bool> not xs@.
newtype TypeApplicator a = TypeApplicator { runTypeApp :: Identity a }
  deriving (Monad)

instance MonadInstantiator TypeApplicator where
  instantiateTApp (ETApp f@(EVar (NTyped _ fScheme)) [instantiatedType]) = 
    return $ applyInstantiatedTypeToInferredScheme instantiatedType fScheme f

  instantiateTApp (ETApp f@(ECon (NTyped _ fScheme)) [instantiatedType]) = 
    return $ applyInstantiatedTypeToInferredScheme instantiatedType fScheme f

applyInstantiatedTypeToInferredScheme :: Type -> Scheme -> Expression -> Expression
applyInstantiatedTypeToInferredScheme instantiatedType scheme exp = 
  case boundInScheme scheme of
    []            -> exp  
    boundInScheme -> 
      let subst    = runIdentity $ unifyLeftOrFail (typeOfScheme scheme) instantiatedType
          typeApps = map (\b -> fromJust $ L.lookup (untypedName b) subst) boundInScheme
      in
        ETApp exp typeApps

-- |Instantiate a scheme to a new type
instantiate :: MonadUnique u => Scheme -> HM u Type
instantiate scheme = case scheme of
  SType t     -> return t
  SForall v x -> do
    t' <- instantiate x
    u' <- newType
    return $ substitute (v,u') t'

-- |Introduce a new type variable
newType :: MonadUnique u => HM u Type
newType = newName "type" >>= return . tVar

-- |Infers the type of an expression
w :: MonadUnique u => Context -> Expression -> HM u ([Substitution], Type, Expression)
w context exp = case exp of
  EVar name -> do scheme  <- lookupName name
                  type_   <- instantiate scheme
                  doIntro <- introduceVarTLamTApp 
                  let exp' = if doIntro
                             then ETApp (EVar $ nTyped name scheme) [type_]
                             else        EVar $ nTyped name scheme
                  return ([], type_, exp')

  ECon name -> do scheme  <- lookupName name
                  type_   <- instantiate scheme
                  doIntro <- introduceConTLamTApp 
                  let exp' = if doIntro
                             then ETApp (ECon $ nTyped name scheme) [type_]
                             else        ECon $ nTyped name scheme
                  return ([], type_, exp')

  EApp f args -> do
    (s1, tArgs, args') <- foldM (\(subst,ts,args') arg -> 
                            do (s,t,a) <- w (substitutes subst context) arg
                               return (subst ++ s, ts ++ [t], args' ++ [a])
                          ) ([],[],[]) args
    (s2, tF, f')  <- w (substitutes s1 context) f
    u             <- newType
    let funT      =  functionType (map (substitutes (s1 ++ s2)) tArgs) u
    
    s3            <- unifyOrFail funT tF 

    let resultT = substitutes s3 u

    return $ (s1 ++ s2 ++ s3, resultT, EApp f' args')

  ELam ns e -> do
    us           <- forM ns $ const newType
    let bindings =  zip ns us
    (s,t,e')     <- w (bindTypes bindings context) e
    let us'      =  map (substitutes s) us
        schemes  =  map SType us'
    return (s, functionType us' t, ELam (zipWith nTyped ns schemes) e')

  ECase e matches -> do
    (eSubst,eType,e') <- w context e
    u                 <- newType

    (subst,resultT,matches') <-
      foldM (\(subst,matchType,matches') match -> 
              let context' = substitutes subst context
                  eType'   = substitutes subst eType
              in do 
                (s',t',match') <- wMatch context' eType' match
                s''            <- unifyOrFail (substitutes s' matchType) t'
                return (subst ++ s' ++ s'', substitutes s'' t', matches' ++ [match'])
            ) (eSubst,u,[]) matches
    
    return (subst,resultT,ECase e' matches')

  ELet bindings localExp -> do
    (subst,  bindings') <- wBindingGroup context bindings

    let context' = bindTypedBindings bindings' context

    (subst', expType, localExp') <- w context' localExp

    return $ (subst ++ subst', expType, ELet bindings' localExp')

  EUndefined -> do type_   <- newType
                   return ([], type_, EUndefined)

  where 
    lookupName name = 
      case (lookup name context, name) of
        (Just s, _)      -> return s
        (_ , NTyped _ s) -> return s
        _                -> fail $ "Algorithms.HindleyMilner.W: "
                                ++ "Can not deduce type for '" 
                                ++ (show $ pprint name) ++ "'"

-- |Annotates the schemes to a program
wProgram :: MonadUnique u => Context -> Program -> HM u Program
wProgram context program = do
  let (typeDecls, valueDecls) = splitDeclarations program
      bgs                     = bindingGroups valueDecls
      context'                = foldr bindAdt context typeDecls

  (_,_,valueDecls') <-  
    foldM (\(s,ctxt,valueDecls') bg -> do
              (s',bg') <- wBindingGroup ctxt bg
              let ctxt' = bindTypedBindings bg' ctxt 
              return (s ++ s', ctxt', bg' ++ valueDecls')
          ) ([],context',[]) bgs

  return $ programFromDeclarations   (mainName program) 
                                   $ typeDecls ++ (map DBind valueDecls')

-- |Annotates the schemes to a mutually recursive binding group 
wBindingGroup :: MonadUnique u => Context -> BindingGroup 
                               -> HM u ([Substitution], BindingGroup)
wBindingGroup context decls = do
  newTypes <- forM decls $ const newType
  doIntro  <- introduceVarTLamTApp 

  let bindings        = zip (map boundName decls) newTypes
      extendedContext = bindTypes bindings context

  (subst,exps') <- 
       foldM (\(subst,exps') (Binding _ exp, newType) -> do
                  (s',t',exp') <- w (substitutes subst extendedContext) exp
                  s''          <- unifyOrFail (substitutes (subst ++ s') newType) t'
                  return (subst ++ s' ++ s'', exps' ++ [exp'])
             ) ([],[]) $ zip decls newTypes

  let context'  = substitutes subst context 
      bindings' = map (\(n, t) -> (n, generalize context' $ substitutes subst t)) bindings
      decls'    = map (\((name, scheme), exp') -> 
                    let exp'' = -- Annotate generalized schemes to recursive calls 
                                -- in this binding group
                                introduceTypedNames (gamma bindings') 
                                                  $ substitutes subst exp' 
                    in case boundInScheme scheme of
                        [] -> Binding (nTyped name scheme) exp''
                        bs -> if doIntro
                              then let bs' = map untypedName bs
                                   in
                                    Binding (nTyped name scheme) $ ETLam bs' exp''
                              else  Binding (nTyped name scheme)             exp''
                  ) $ zip bindings' exps' 

  return (subst, decls')

-- |Infers the type of a case's match. The type of the pattern-matched expression 
-- @matchedType@ must be provided in order to restrict the pattern's type.
wMatch :: MonadUnique u => Context -> Type -> Match -> HM u ([Substitution], Type, Match)
wMatch context matchedType (Match pat exp) = do
  (patType,bindings,pat') <- wPat context pat
  s                       <- unifyOrFail patType matchedType 
  (s',t,exp')             <- w (bindTypes (substitutes s bindings) context) exp
  return (s ++ s', t, Match pat' exp')

-- |Infers the type of expressions that a given pattern matches. 
-- Also returns the bindings of names that are bound in the pattern.
wPat :: MonadUnique u => Context -> Pattern -> HM u (Type,[(Name,Type)],Pattern)
wPat context pat = case pat of
  PVar var -> do t <- newType
                 return (t, [(var, t)], PVar $ nTyped var $ sType t)

  PCon n ps -> do
    (bindings,ps')  <- foldM (\(bindings,ps') p -> do
                          (_,bs,p') <- wPat context p
                          return (bindings ++ bs, ps' ++ [p'])
                       ) ([],[]) ps

    let exp = patternToExpression $ PCon n ps'
    (subst,type_,exp') <- withoutTLamTApp $ w (bindTypes bindings context) exp

    let bindings' = map (\(n,b) -> (n, substitutes subst b)) bindings
        pat'      = substitutes subst $ expressionToPattern exp'
    return (type_, bindings', pat')

  where withoutTLamTApp = local (const $ HMConfig DontIntroduceTLamTApp False)

