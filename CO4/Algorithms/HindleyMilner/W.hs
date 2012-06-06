{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.HindleyMilner.W
  (HMConfig (..), schemeOfExp, schemes)
where
  
import           Prelude hiding (lookup)
import           Control.Monad.Identity
import           Control.Monad.Reader
import qualified Data.List as L
import           Data.Maybe (fromJust)
import           Data.Graph (stronglyConnComp,flattenSCC)
import           CO4.Util (topLevelNames)
import           CO4.Language
import           CO4.Unique (MonadUnique,newName')
import           CO4.Names
import           CO4.TypesUtil
import           CO4.Algorithms.HindleyMilner.Util
import           CO4.Algorithms.Bound (bound)
import           CO4.PPrint
import           CO4.Algorithms.Instantiator hiding (instantiate)
import           CO4.Algorithms.TypedNames (typedNames)

data HMConfig     = HMConfig { introduceTLamTApp :: Bool }
type HM u a       = ReaderT HMConfig u a
type BindingGroup = [Declaration]

runHm :: MonadUnique u => HMConfig -> HM u a -> u a
runHm = flip runReaderT

-- |Infers the scheme of an expression without introducing type abstractions
-- and type applications
schemeOfExp :: MonadUnique u => Context -> Expression -> u Scheme
schemeOfExp context exp = do
  (subst,t,_) <- runHm (HMConfig False) $ w context exp
  let t' = generalize (substitutes subst context) t
  return t'

-- |Annotates the scheme to all named expressions/declarations 
schemes :: MonadUnique u => HMConfig -> Context -> Program -> u Program
schemes hmConfig context program = do
  program' <- runHm hmConfig $ wProgram context program
  return $ runIdentity $ runTypeApp $ instantiateProgram program'

-- | While introducing explicit type applications, we apply an instantiated
-- type @t@ to polymorphic function symbols in the first place:
-- @map not xs -> map <(Bool -> Bool) -> [Bool] -> [Bool]> not xs@
-- @TypeApplicator@ unifies @t@ with the general type of @map@.
-- The final type application consists of the substituted variables:
-- @map <Bool,Bool> not xs@.
newtype TypeApplicator a = TypeApplicator { runTypeApp :: Identity a }
  deriving (Functor, Monad)

instance MonadInstantiator TypeApplicator where
  instantiateTApp (ETApp f@(EVar (TypedName _ fScheme)) [instantiatedType]) = 
    if isFunType instantiatedType 
    then case (bound fScheme, free instantiatedType) of
      ([],[]) -> return f
      ([],fs) -> -- Nothing to unify: type app consists of all free variables
        let typeApps = map TVar fs
        in
          return $ ETApp f typeApps

      (bs,_) -> 
        let fType    = typeOfScheme fScheme
            subst    = runIdentity $ unifyLeftOrFail fType instantiatedType
            typeApps = map (\b -> fromJust $ L.lookup b subst) bs
        in
          return $ ETApp f typeApps
    else return f

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
newType = newName' "type" >>= return . TVar

-- |Infers the type of an expression
w :: MonadUnique u => Context -> Expression -> HM u ([Substitution], Type, Expression)
w context exp = case exp of
  EVar name -> do scheme  <- lookupName name
                  type_   <- instantiate scheme
                  doIntro <- asks introduceTLamTApp 
                  let exp' = if doIntro
                             then ETApp (EVar $ typedName name scheme) [type_]
                             else        EVar $ typedName name scheme
                  return ([], type_, exp')

  ECon name -> do scheme <- lookupName name
                  type_  <- instantiate scheme
                  return ([], type_, ECon $ typedName name scheme)

  ELit lit  -> return ([], wLit lit, exp)

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

    return (s, functionType us' t, ELam (zipWith typedName ns schemes) e')

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

  ELet name value localExp -> do
    (subst,  [DBind name' value']) <- wBindingGroup context [DBind name value]

    let TypedName _ valueScheme = name'
    (subst', expType, localExp') <- w (binds [(name,valueScheme)] context) localExp

    return $ (subst ++ subst', expType, ELet name' value' localExp')

  where 
    lookupName name = 
      case (lookup name context, name) of
        (Just s, _)         -> return s
        (_ , TypedName _ s) -> return s
        _                   -> fail $ "Can not deduce type for '" ++ (show $ pprint name) ++ "'"

-- |Annotates the schemes to a program
wProgram :: MonadUnique u => Context -> Program -> HM u Program
wProgram context program = do
  let bgs = bindingGroups program

  (_,_,program') <-
    foldM (\(s,ctxt,program') bg -> do 
              (s',bg')  <- wBindingGroup ctxt bg
              let ctxt' =  gamma $ map (\(DBind (TypedName n s) _) -> (Name n, s)) bg'
              return (s ++ s', mappend ctxt ctxt', program' ++ bg')
          ) ([],context,[]) bgs

  return program'

-- |Annotates the schemes to a mutually recursive binding group 
wBindingGroup :: MonadUnique u => Context -> BindingGroup 
                               -> HM u ([Substitution], BindingGroup)
wBindingGroup context decls = do
  types   <- forM decls $ const newType
  doIntro <- asks introduceTLamTApp 

  let bindings        = zip (topLevelNames decls) types
      extendedContext = bindTypes bindings context

  (subst,exps') <- 
       foldM (\(subst,exps') (decl, typeVar) -> do
                  (s',t',exp') <- wDecl (substitutes subst extendedContext) decl
                  s''          <- unifyOrFail (substitutes (subst ++ s') typeVar) t'
                  return (subst ++ s' ++ s'', exps' ++ [exp'])
             ) ([],[]) $ zip decls types

  let context'  = substitutes subst context 
      bindings' = map (\(n, t) -> (n, generalize context' $ substitutes subst t)) bindings
      decls'    = map (\((name, scheme), exp') -> 
                    let exp'' = -- Annotate generalized schemes to recursive calls 
                                -- in this binding group
                            typedNames (gamma bindings') $ substitutes subst exp' 
                    in case bound scheme of
                        [] -> DBind (typedName name scheme) exp''
                        bs -> if doIntro
                              then DBind (typedName name scheme) $ ETLam bs exp''
                              else DBind (typedName name scheme)            exp''
                  ) $ zip bindings' exps' 

  return (subst, decls')

  where wDecl extendedContext (DBind (TypedName _ scheme) exp) = do
          providedType               <- instantiate scheme
          (subst',inferredType,exp') <- w extendedContext exp 
          subst''                    <- unifyLeftOrFail inferredType providedType

          let inferredType' = substitutes subst'' inferredType

          return (subst' ++ subst'', inferredType', exp')

        wDecl extendedContext (DBind _ exp) = w extendedContext exp 

-- |Infers the type of a case's match. The type of the pattern-matched expression 
-- @matchedType@ must be provided in order to restrict the pattern's type.
wMatch :: MonadUnique u => Context -> Type -> Match -> HM u ([Substitution], Type, Match)
wMatch context matchedType (Match pat exp) = do
  (patType,bindings)      <- wPat context pat
  s                       <- unifyOrFail patType matchedType 
  (s',t,exp')             <- w (bindTypes (substitutes s bindings) context) exp
  return (s ++ s', t, Match pat exp')

-- |Infers the type of a pattern. Also returns the bindings of
-- names that are bound in the pattern
wPat :: MonadUnique u => Context -> Pattern -> HM u (Type,[(Name,Type)])
wPat context pat = case pat of
  PLit lit -> return (wLit lit, [])
  PVar var -> do t <- newType
                 return (t, [(var, t)])

  PCon _ ps -> do
    bindings <- foldM (\bindings p -> do
                          (_,bs) <- wPat context p
                          return $ bindings ++ bs
                       ) [] ps

    (subst,type_,_) <- w (bindTypes bindings context) $ pat2exp pat

    let bindings' = map (\(n,b) -> (n, substitutes subst b)) bindings
    return (type_, bindings')

    where 
      pat2exp (PVar name)    = EVar name
      pat2exp (PLit lit)     = ELit lit
      pat2exp (PCon name []) = ECon name
      pat2exp (PCon name ps) = EApp (ECon name) $ map pat2exp ps
 
-- |Infers the type of a literal
wLit :: Literal -> Type
wLit lit = case lit of
  LInt _     -> TCon intType []
  LChar _    -> TCon charType []
  LDouble _  -> TCon doubleType []

-- |Computes groups of mutually recursive declarations
bindingGroups :: Program -> [BindingGroup]
bindingGroups program =
  let graph = map (\d@(DBind n e) -> (d, untypedName n, free e)) program
  in
    map (L.nub . flattenSCC) $ stronglyConnComp graph

