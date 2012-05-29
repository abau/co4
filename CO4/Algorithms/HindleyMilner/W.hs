{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.HindleyMilner.W
  (schemeOfExp, schemes)
where
  
import           Prelude hiding (lookup)
import           Control.Monad.Identity
import           Control.Applicative ((<$>))
import qualified Data.List as L
import           Data.Maybe (fromJust)
import           Data.Graph (stronglyConnComp,flattenSCC)
import           CO4.Util (topLevelNames,splitDeclarations)
import           CO4.Language
import           CO4.Unique (Unique,newName)
import           CO4.Names
import           CO4.TypesUtil
import           CO4.Algorithms.HindleyMilner.Util
import           CO4.Algorithms.Bound (bound)
import           CO4.PPrint
import           CO4.Algorithms.Instantiator hiding (instantiate)
import           CO4.Algorithms.TypedNames (typedNames)

type HM a         = Unique a
type BindingGroup = [Declaration]

runHm :: HM a -> Unique a
runHm = id 

-- |Infers the scheme of an expression
schemeOfExp :: Context -> Expression -> Unique Scheme
schemeOfExp context exp = do
  (subst,t,_) <- runHm $ w context exp
  let t' = generalize (substitutes subst context) t
  return t'

-- |Infers the scheme of all bound expressions/declarations 
schemes :: Context -> Program -> Unique Program
schemes context program = do
  (_,program') <- runHm (wProgram context program)
  return $ runIdentity $ runTypeApp $ instantiateProgram program'

newtype TypeApplicator a = TypeApplicator { runTypeApp :: Identity a }
  deriving (Functor, Monad)

instance MonadInstantiator TypeApplicator where
  instantiateTApp (ETApp f@(EVar (NTyped _ fScheme)) [instantiatedType]) = 
    if isFunType instantiatedType 
    then case (bound fScheme, free instantiatedType) of
      ([],[]) -> return f
      ([],fs) -> 
        let typeApps = map (TVar . untypedName) fs
        in
          return $ ETApp f typeApps

      (bs,_) -> 
        let fType    = typeOfScheme fScheme
            subst    = runIdentity $ unifyLeftOrFail fType instantiatedType
            bs'      = map untypedName bs
            typeApps = map (\b -> fromJust $ L.lookup b subst) bs'
        in
          return $ ETApp f typeApps

    else return f

instantiate :: Scheme -> HM Type
instantiate scheme = case scheme of
  SType t     -> return t
  SForall v x -> do
    t' <- instantiate x
    u' <- newType
    return $ substitute (v,u') t'

newType :: HM Type
newType = TVar <$> (newName "type")

-- |Infers the type of an expression
w :: Context -> Expression -> HM ([Substitution], Type, Expression)
w context exp = case exp of
  EVar name -> do scheme <- lookupName name
                  type_  <- instantiate scheme
                  return ([], type_, ETApp (EVar $ nTyped name scheme) [type_])

  ECon name -> do scheme <- lookupName name
                  type_  <- instantiate scheme
                  return ([], type_, ECon $ nTyped name scheme)

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

    return (s, functionType us' t, ELam (zipWith (\n -> nTyped n . SType) ns us') e')

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

    let NTyped _ valueScheme = name'
    (subst', expType, localExp') <- w (binds [(name,valueScheme)] context) localExp

    return $ (subst ++ subst', expType, ELet name' value' localExp')

  where 
    lookupName name = 
      case (lookup name context, name) of
        (Just s, _)      -> return s
        (_ , NTyped _ s) -> return s
        _                -> fail $ "Can not deduce type for '" ++ (show $ pprint name) ++ "'"

-- |Infers the toplevel types of a program (returned as a context)
wProgram :: Context -> Program -> HM ([Substitution], Program)
wProgram context program = do
  let (typeDecls, valueDecls) = splitDeclarations program
      bgs                     = bindingGroups valueDecls
      context'                = foldr bindAdt context typeDecls

  (subst,_,valueDecls') <-
    foldM (\(s,ctxt,program') bg -> do 
              (s',bg')  <- wBindingGroup ctxt bg
              let ctxt' =  gamma $ map (\(DBind (NTyped n s) _) -> (UntypedName n, s)) bg'
              return (s ++ s', mappend ctxt ctxt', program' ++ bg')
          ) ([],context',[]) bgs

  return (subst, typeDecls ++ valueDecls')

-- |Infers the types of a mutually recursive binding group 
wBindingGroup :: Context -> BindingGroup -> HM ([Substitution], BindingGroup)
wBindingGroup context decls = do
  types <- forM decls $ const newType

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
                    let exp'' = -- Introduce generalized schemes to recursive calls 
                                -- in this binding group
                            typedNames (gamma bindings') $ substitutes subst exp' 
                    in case bound scheme of
                        [] -> DBind (nTyped name scheme) exp''
                        bs -> 
                          let bs' = map untypedName bs
                          in
                            DBind (nTyped name scheme) $ ETLam bs' exp''
                  ) $ zip bindings' exps' 

  return (subst, decls')

  where wDecl extendedContext (DBind (NTyped _ scheme) exp) = do
          providedType               <- instantiate scheme
          (subst',inferredType,exp') <- w extendedContext exp 
          subst''                    <- unifyLeftOrFail inferredType providedType

          let inferredType' = substitutes subst'' inferredType

          return (subst' ++ subst'', inferredType', exp')

        wDecl extendedContext (DBind _ exp) = w extendedContext exp 

-- |Infers the type of a case's match. The type of the pattern-matched expression 
-- @matchedType@ must be provided in order to restrict the pattern's type.
wMatch :: Context -> Type -> Match -> HM ([Substitution], Type, Match)
wMatch context matchedType (Match pat exp) = do
  (patType,bindings)      <- wPat context pat
  s                       <- unifyOrFail patType matchedType 
  (s',t,exp')             <- w (bindTypes (substitutes s bindings) context) exp
  return (s ++ s', t, Match pat exp')

-- |Infers the type of a pattern. Also returns the bindings of
-- names that are bound in the pattern
wPat :: Context -> Pattern -> HM (Type,[(Name,Type)])
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
  let graph = map (\d@(DBind n e) -> (d, untypedName n, map untypedName $ free e)) program
  in
    map (L.nub . flattenSCC) $ stronglyConnComp graph

