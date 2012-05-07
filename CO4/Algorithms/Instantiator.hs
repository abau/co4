module CO4.Algorithms.Instantiator
  (MonadInstantiator(..), Instantiable (..))
where

import CO4.Language

class Monad m => MonadInstantiator m where

  -- |Instantiate schemes. Default is @return@
  instantiateScheme :: Scheme -> m Scheme
  instantiateScheme = return

  -- |Instantiate types. Default is @return@
  instantiateType :: Type -> m Type
  instantiateType = return

  -- |Instantiate names. Default instantiates scheme of typed-names.
  instantiateName :: Name -> m Name
  instantiateName (TypedName n s) = do
    s' <- instantiate s
    return $ TypedName n s'

  instantiateName name = return name

  -- |Instantiates variables.
  -- Default instantiates name.
  instantiateVar :: Expression -> m Expression
  instantiateVar (EVar n) = do
    n' <- instantiate n
    return $ EVar n'

  -- |Instantiates constructors.
  -- Default instantiates name.
  instantiateCon :: Expression -> m Expression
  instantiateCon (ECon n) = do
    n' <- instantiate n
    return $ ECon n'

  -- |Instantiates literals.
  -- Default is @return@.
  instantiateLit :: Expression -> m Expression
  instantiateLit = return

  -- |Instantiates function applications @f xs@. 
  -- Default instantiates sub-expressions and calls @postprocessLambdaApp@ if
  -- @f@ results in a lambda expression
  instantiateApp :: Expression -> m Expression
  instantiateApp (EApp f args) = do
    f'    <- instantiate f
    args' <- instantiate args
    case f' of
      ELam {} -> postprocessLambdaApp f' args'
      _       -> return $ EApp f' args'
    
  -- |@postprocessLambdaApp (\...->...) args@ postprocesses a lambda application 
  -- resulting from @instantiateApp@. Default is @return $ EApp f args@
  postprocessLambdaApp :: Expression -> [Expression] -> m Expression
  postprocessLambdaApp f args = return $ EApp f args
    
  -- |Instantiates type application. Default instantiates sub-expression and types.
  instantiateTApp :: Expression -> m Expression
  instantiateTApp (ETApp e types) = do
    e'     <- instantiate e
    types' <- instantiate types
    return $ ETApp e' types'

  -- |Instantiates let-expressions. Default instantiates sub-expressions and name.
  instantiateLet :: Expression -> m Expression
  instantiateLet (ELet n v e) = do
    n' <- instantiate n
    v' <- instantiate v
    e' <- instantiate e
    return $ ELet n' v' e'

  -- |Instantiates lambda-expressions. Default instantiates sub-expression and names.
  instantiateLam :: Expression -> m Expression
  instantiateLam (ELam ns e) = do
    ns' <- instantiate ns
    e'  <- instantiate e 
    return $ ELam ns' e'

  -- |Instantiates type-abstraction. Default instantiates sub-expression and names.
  instantiateTLam :: Expression -> m Expression
  instantiateTLam (ETLam ns e) = do
    ns' <- instantiate ns
    e'  <- instantiate e 
    return $ ETLam ns' e'

  -- |Instantiates case-expressions. Default instantiates the matched expression 
  -- and all matches
  instantiateCase :: Expression -> m Expression
  instantiateCase (ECase e ms) = do
    e'  <- instantiate e
    ms' <- instantiate ms
    return $ ECase e' ms'

  -- |Instantiates pattern-match. Default instantiates pattern and subexpression.
  instantiateMatch :: Match -> m Match
  instantiateMatch (Match p e) = do
    p' <- instantiate p
    e' <- instantiate e 
    return $ Match p' e'

  -- |Instantiates program. Default instantiates declarations.
  instantiateProgram :: Program -> m Program
  instantiateProgram = mapM instantiate

  -- |Instantiates declaration. Default instantiates name and subexpression.
  instantiateDeclaration :: Declaration -> m Declaration
  instantiateDeclaration (DBind name exp) = do
    name' <- instantiate name
    exp'  <- instantiate exp
    return $ DBind name' exp' 

  -- |Instantiates expression. Default calls @instantiate...@ according
  -- to the matched constructor.
  instantiateExpression :: Expression -> m Expression
  instantiateExpression exp = case exp of
    EVar {}  -> instantiateVar exp
    ECon {}  -> instantiateCon exp
    ELit {}  -> instantiateLit exp
    EApp {}  -> instantiateApp exp
    ETApp {} -> instantiateTApp exp
    ELam {}  -> instantiateLam exp
    ETLam {} -> instantiateTLam exp
    ECase {} -> instantiateCase exp
    ELet {}  -> instantiateLet exp

  -- |Instantiates pattern. Default instantiates names.
  instantiatePattern :: Pattern -> m Pattern
  instantiatePattern pat = case pat of
    PVar n    -> instantiate n >>= return . PVar
    PCon n ps -> do
      n'  <- instantiate n
      ps' <- instantiate ps
      return $ PCon n' ps'

class Instantiable a where
  instantiate :: MonadInstantiator m => a -> m a

instance Instantiable Name where
  instantiate = instantiateName

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

instance Instantiable Declaration where
  instantiate = instantiateDeclaration

instance (Instantiable a) => Instantiable [a] where
  instantiate = mapM instantiate
