{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module CO4.Algorithms.HindleyMilner.W
  {-
  ( HMConfig (..), IntroduceTLamTApp (..) 
  , schemeOfExp, schemesConfig, schemes, withSchemes)
  -}
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
import           CO4.Algorithms.Util (introduceTypedNames)
import           CO4.PPrint
import           CO4.Algorithms.Instantiator hiding (instantiate)
import           CO4.Algorithms.Util (eraseTypedNames)
import           CO4.Algorithms.TopologicalSort (bindingGroups)
import           CO4.Prelude (unparsedPreludeContext)
import           CO4.Config (MonadConfig,is,Config(ImportPrelude))

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

initialContext :: MonadConfig m => m Context
initialContext = do
  is ImportPrelude >>= \case
    False -> return emptyContext
    True  -> return unparsedPreludeContext

-- |Infers the scheme of an expression without introducing type abstractions
-- and type applications
schemeOfExp :: (MonadUnique u,MonadConfig u) => Expression -> u Scheme
schemeOfExp exp = do
  context     <- initialContext
  (subst,t,_) <- runHm (HMConfig DontIntroduceTLamTApp False) $ w context exp
  let t' = generalize (substituteN subst context) t
  return t'

-- |Annotates the scheme to all named expressions/declarations 
schemes :: (MonadUnique u,MonadConfig u) => Program -> u Program
schemes program = do
  context <- initialContext 
  schemesConfig (HMConfig DontIntroduceTLamTApp True) context program

-- |@withSchemes f p@ applies @f@ to @p'@, where @p'@ equals @p@ after type inference.
-- Returns the result of @f p'@ with erased type information.
withSchemes :: (MonadConfig u,MonadUnique u) => (Program -> u Program) 
                                             -> Program -> u Program
withSchemes f program = schemes program >>= f >>= return . eraseTypedNames

schemesConfig :: MonadUnique u => HMConfig -> Context -> Program -> u Program
schemesConfig hmConfig context program = do
  program' <- runHm hmConfig $ wProgram context program

  let schemeOfMain = schemeOfName $ boundName $ pMain program'

  if (null $ boundInScheme schemeOfMain) || (not $ errorOnPolymorphicMain hmConfig)
    then return $ runIdentity $ runTypeApp $ instantiateProgram program'
    else error $ "Algorithms.HindleyMilner: main-constraint must not have the polymorphic type '" ++ show (pprint schemeOfMain) ++ "'"

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
      let subst    = runIdentity $ unifyOrFail (typeOfScheme scheme) instantiatedType
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
w context = \case 
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

  EApp e args -> do
    (s1, argsT, args', context') <- foldM (\(s1,argsT,args',context') arg -> do
                                      (s2,argT,arg') <- w context' arg
                                      return ( s1    ++ s2
                                             , argsT ++ [argT]
                                             , args' ++ [arg']
                                             , substituteN s2 context' )
                                    ) ([],[],[],context) args
    (s2, eT, e') <- w (substituteN s1 context') e
    resultT      <- newType
    let funT     =  functionType (map (substituteN (s1 ++ s2)) argsT) resultT
    s3           <- unifyOrFail funT eT
    let appT     =  substituteN s3 resultT
    return (s1 ++ s2 ++ s3, appT, EApp e' args')

  ELam ns e -> do
    nsT           <- forM ns $ const newType
    let bindings  =  zip ns nsT
    (s,eT,e')     <- w (bindTypes bindings context) e
    let nsT'      =  map (substituteN s) nsT
        nsSchemes =  map SType nsT'
    return (s, functionType nsT' eT, ELam (zipWith nTyped ns nsSchemes) e')

  ECase e matches -> do
    (s1, eT, e') <- w context e
    (s2, patsT, branchesT, matches', _) <- 
      foldM (\(s2,patsT,branchesT,matches',context') match -> do
        (s3,patT,branchT,match') <- wMatch context' match
        return ( s2        ++ s3
               , patsT     ++ [patT]
               , branchesT ++ [branchT]
               , matches'  ++ [match']
               , substituteN s3 context'
               )
      ) ([],[],[],[], substituteN s1 context) matches
    
    resultT <- newType
    s3      <- unifyNorFail $ substituteN (s1 ++ s2)       (eT      : patsT)
    s4      <- unifyNorFail $ substituteN (s1 ++ s2 ++ s3) (resultT : branchesT)

    let s5 = concat [s1,s2,s3,s4]
    return (s5, substituteN s5 resultT, ECase e' matches')

  ELet bindings e -> do
    (s1,bindings') <- wBindingGroup context bindings
    let context'   =  bindTypedBindings bindings' context
    (s2, eT, e')   <- w context' e

    return $ (s1 ++ s2, eT, ELet bindings' e')

  EUndefined -> do 
    t <- newType
    return ([],t,EUndefined)
  where 
    lookupName name = 
      case (lookup name context, name) of
        (Just s, _)      -> return s
        (_ , NTyped _ s) -> return s
        _ -> if isInt (fromName name) 
             then return $ SType $ TCon intName []
             else fail $ "Algorithms.HindleyMilner.W: " 
                      ++ "Can not deduce type for '" ++ (show $ pprint name) ++ "'"

-- |Annotates the schemes to a program
wProgram :: MonadUnique u => Context -> Program -> HM u Program
wProgram context program = do
  let (typeDecls, valueDecls) = splitDeclarations program
      bgs                     = bindingGroups valueDecls
      context'                = foldr bindAdt context typeDecls

  (_,_,valueDecls') <-  
    foldM (\(s1,context,valueDecls') bg -> do
              (s2,bg')     <- wBindingGroup context bg
              let context' =  bindTypedBindings bg' context
              return (s1 ++ s2, context', valueDecls' ++ bg')
          ) ([],context',[]) bgs

  return $ programFromDeclarations $ concat [ map DAdt typeDecls 
                                            , map DBind valueDecls'
                                            ]

-- |Annotates the schemes to a mutually recursive binding group 
wBindingGroup :: MonadUnique u => Context -> BindingGroup 
                               -> HM u ([Substitution], BindingGroup)
wBindingGroup context decls = do
  rhsTs   <- forM decls $ const newType
  doIntro <- introduceVarTLamTApp 

  let names           = map boundName decls
      extendedContext = bindTypes (zip names rhsTs) context

  (s1,rhss',_) <- 
       foldM (\(s1,rhss',extendedContext) (Binding _ rhs, rhsT) -> do
                  (s2,rhsT',rhs') <- w extendedContext rhs
                  s3              <- unifyOrFail (substituteN (s1 ++ s2) rhsT) rhsT'
                  return ( concat [s1,s2,s3]
                         , rhss' ++ [rhs']
                         , substituteN (s2 ++ s3) extendedContext
                         )
             ) ([],[],extendedContext) 
               (zip decls rhsTs)

  let genRhsTs  = map (generalize (substituteN s1 context) . substituteN s1) rhsTs
      rhss''    = map (generalizeRecursiveCalls . substituteN s1) rhss'

      generalizeRecursiveCalls = introduceTypedNames $ gamma $ zip names genRhsTs
                  
      decls'    = zipWith3 makeBinding names genRhsTs rhss''

      makeBinding name scheme rhs =
        case boundInScheme scheme of
          [] -> Binding (nTyped name scheme) rhs
          bs -> if doIntro
                then let bs' = map untypedName bs
                     in
                      Binding (nTyped name scheme) $ ETLam bs' rhs
                else  Binding (nTyped name scheme)             rhs

  return (s1, decls')

-- |Infers two types for a match: 
-- 1. the type of expressions the pattern is able to match on
-- 2. the type of the match's branch
wMatch :: MonadUnique u => Context -> Match 
                        -> HM u ([Substitution], Type, Type, Match)
wMatch context (Match pat exp) = do
  (patT,bindings,pat') <- wPat context pat
  (s,expT,exp')        <- w (bindTypes bindings context) exp
  return (s, patT, expT, Match pat' exp')

-- |Infers the type of expression a pattern is able to match on. Also returns
-- the bindings in the pattern.
wPat :: MonadUnique u => Context -> Pattern -> HM u (Type,[(Name,Type)],Pattern)
wPat context pat = case pat of
  PVar var -> do t <- newType
                 return (t, [(var, t)], PVar $ nTyped var $ sType t)

  PCon n ps -> do
    (_,bindingss,ps')  <- mapAndUnzip3M (wPat context) ps

    let bindings = concat bindingss
        exp      = patternToExpression $ PCon n ps'

    (subst,type_,exp') <- withoutTLamTApp $ w (bindTypes bindings context) exp

    let bindings' = map (\(n,b) -> (n, substituteN subst b)) bindings
        pat'      = substituteN subst $ expressionToPattern exp'
    return (type_, bindings', pat')

  where withoutTLamTApp = local (const $ HMConfig DontIntroduceTLamTApp False)
