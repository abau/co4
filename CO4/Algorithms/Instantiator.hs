module CO4.Algorithms.Instantiator
  (MonadInstantiator(..), Instantiable(..), instantiateSubexpressions)
where

import CO4.Language

class Monad m => MonadInstantiator m where

  instantiateScheme :: Scheme -> m Scheme
  instantiateScheme = return

  instantiateType :: Type -> m Type
  instantiateType = return

  instantiateUntypedName :: UntypedName -> m UntypedName
  instantiateUntypedName = return

  instantiateName :: Name -> m Name
  instantiateName (NTyped n s) = do
    s' <- instantiate s
    return $ NTyped n s'
  instantiateName name = return name

  instantiatePattern :: Pattern -> m Pattern
  instantiatePattern pat = case pat of
    PVar n    -> instantiate n >>= return . PVar
    PCon n ps -> do
      n'  <- instantiate n
      ps' <- instantiate ps
      return $ PCon n' ps'

  instantiateMatch :: Match -> m Match
  instantiateMatch (Match p e) = do
    p' <- instantiate p
    e' <- instantiate e 
    return $ Match p' e'

  instantiateBinding :: Binding -> m Binding
  instantiateBinding (Binding n e) = do
    n' <- instantiate n
    e' <- instantiate e
    return $ Binding n' e'

  instantiateVar :: Expression -> m Expression
  instantiateVar (EVar n) = do
    n' <- instantiate n
    return $ EVar n'

  instantiateCon :: Expression -> m Expression
  instantiateCon (ECon n) = do
    n' <- instantiate n
    return $ ECon n'

  instantiateApp :: Expression -> m Expression
  instantiateApp (EApp f args) = do
    f'    <- instantiate f
    args' <- instantiate args
    return $ EApp f' args'

  instantiateTApp :: Expression -> m Expression
  instantiateTApp (ETApp e types) = do
    e'     <- instantiate e
    types' <- instantiate types
    return $ ETApp e' types'

  instantiateLam :: Expression -> m Expression
  instantiateLam (ELam ns e) = do
    ns' <- instantiate ns
    e'  <- instantiate e 
    return $ ELam ns' e'

  instantiateTLam :: Expression -> m Expression
  instantiateTLam (ETLam ns e) = do
    ns' <- instantiate ns
    e'  <- instantiate e 
    return $ ETLam ns' e'

  instantiateCase :: Expression -> m Expression
  instantiateCase (ECase e ms) = do
    e'  <- instantiate e
    ms' <- instantiate ms
    return $ ECase e' ms'

  instantiateLet :: Expression -> m Expression
  instantiateLet (ELet b e) = do
    b' <- instantiate b
    e' <- instantiate e
    return $ ELet b' e'

  instantiateUndefined :: m Expression
  instantiateUndefined = return EUndefined

  instantiateExpression :: Expression -> m Expression
  instantiateExpression = instantiateSubexpressions

  instantiateConstructor :: Constructor -> m Constructor
  instantiateConstructor (CCon name types) = do
    name'  <- instantiate name
    types' <- instantiate types
    return $ CCon name' types'

  instantiateBind :: Declaration -> m Declaration
  instantiateBind (DBind b) = instantiateBinding b >>= return . DBind

  instantiateAdt :: Adt -> m Adt
  instantiateAdt (Adt name ts cons) = do
    name' <- instantiate name
    ts'   <- instantiate ts
    cons' <- instantiate cons
    return $ Adt name' ts' cons'

  instantiateDeclaration :: Declaration -> m Declaration
  instantiateDeclaration decl = case decl of
    DBind {} -> instantiateBind decl
    DAdt adt -> instantiateAdt adt >>= return . DAdt

  instantiateMain :: Binding -> m Binding
  instantiateMain main = do
    DBind main' <- instantiateDeclaration $ DBind main
    return main'

  instantiateProgram :: Program -> m Program
  instantiateProgram (Program main decls) = do
    main'  <- instantiateMain main
    decls' <- instantiate     decls
    return $ Program main' decls'

instantiateSubexpressions :: MonadInstantiator m => Expression -> m Expression
instantiateSubexpressions exp = case exp of
  EVar {}    -> instantiateVar exp
  ECon {}    -> instantiateCon exp
  EApp {}    -> instantiateApp exp
  ETApp {}   -> instantiateTApp exp
  ELam {}    -> instantiateLam exp
  ETLam {}   -> instantiateTLam exp
  ECase {}   -> instantiateCase exp
  ELet {}    -> instantiateLet exp
  EUndefined -> instantiateUndefined

class Instantiable a where
  instantiate :: MonadInstantiator m => a -> m a

instance Instantiable Name where
  instantiate = instantiateName

instance Instantiable UntypedName where
  instantiate = instantiateUntypedName

instance Instantiable Type where
  instantiate = instantiateType

instance Instantiable Scheme where
  instantiate = instantiateScheme

instance Instantiable Expression where
  instantiate = instantiateExpression

instance Instantiable Pattern where
  instantiate = instantiatePattern

instance Instantiable Match where
  instantiate = instantiateMatch

instance Instantiable Binding where
  instantiate = instantiateBinding

instance Instantiable Adt where
  instantiate = instantiateAdt

instance Instantiable Declaration where
  instantiate = instantiateDeclaration

instance Instantiable Constructor where
  instantiate = instantiateConstructor

instance Instantiable Program where
  instantiate = instantiateProgram

instance (Instantiable a) => Instantiable [a] where
  instantiate = mapM instantiate
