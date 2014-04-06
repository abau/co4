module CO4.Algorithms.Collector
  (MonadCollector(..), Collectable (..))
where

import CO4.Language

class Monad m => MonadCollector m where

  collectUntypedName :: UntypedName -> m ()
  collectUntypedName = const $ return ()

  collectType :: Type -> m ()
  collectType t = case t of
    TVar v       -> collect v
    TCon c ts    -> collect c >> collect ts

  collectScheme :: Scheme -> m ()
  collectScheme scheme = case scheme of
    SType t     -> collect t
    SForall n s -> collect n >> collect s

  collectName :: Name -> m ()
  collectName (NTyped _ s) = collect s
  collectName _            = return ()

  collectPattern :: Pattern -> m ()
  collectPattern pat = case pat of
    PVar n    -> collect n 
    PCon n ps -> collect n >> collect ps

  collectMatch :: Match -> m ()
  collectMatch (Match p e) = collect p >> collect e 

  collectBinding :: Binding -> m ()
  collectBinding (Binding n e) = collect n >> collect e

  collectVar :: Expression -> m ()
  collectVar (EVar n) = collect n

  collectCon :: Expression -> m ()
  collectCon (ECon n) = collect n
    
  collectApp :: Expression -> m ()
  collectApp (EApp f args) = collect f >> collect args

  collectTApp :: Expression -> m ()
  collectTApp (ETApp e types) = collect e >> collect types

  collectLam :: Expression -> m ()
  collectLam (ELam ns e) = collect ns >> collect e 

  collectTLam :: Expression -> m ()
  collectTLam (ETLam ns e) = collect ns >> collect e 

  collectCase :: Expression -> m ()
  collectCase (ECase e ms) = collect e >> collect ms

  collectLet :: Expression -> m ()
  collectLet (ELet b e) = collect b >> collect e

  collectExpression :: Expression -> m ()
  collectExpression exp = case exp of
    EVar {}    -> collectVar exp
    ECon {}    -> collectCon exp
    EApp {}    -> collectApp exp
    ETApp {}   -> collectTApp exp
    ELam {}    -> collectLam exp
    ETLam {}   -> collectTLam exp
    ECase {}   -> collectCase exp
    ELet {}    -> collectLet exp

  collectConstructor :: Constructor -> m ()
  collectConstructor (CCon name types) = collect name >> collect types
    
  collectBind :: Declaration -> m ()
  collectBind (DBind binding) = collect binding

  collectAdt :: Adt -> m ()
  collectAdt (Adt name ts cons) = collect name >> collect ts >> collect cons

  collectSignature :: Signature -> m ()
  collectSignature (Signature name scheme) = collect name >> collect scheme

  collectDeclaration :: Declaration -> m ()
  collectDeclaration decl = case decl of
    DBind {} -> collectBind decl
    DAdt adt -> collect adt
    DSig sig -> collect sig

  collectMain :: Binding -> m ()
  collectMain = collectBinding

  collectProgram :: Program -> m ()
  collectProgram (Program main decls) = collectMain main >> collect decls

class Collectable a where
  collect :: MonadCollector m => a -> m ()

instance Collectable UntypedName where
  collect = collectUntypedName

instance Collectable Type where
  collect = collectType

instance Collectable Scheme where
  collect = collectScheme

instance Collectable Name where
  collect = collectName

instance Collectable Expression where
  collect = collectExpression

instance Collectable Pattern where
  collect = collectPattern

instance Collectable Match where
  collect = collectMatch

instance Collectable Binding where
  collect = collectBinding

instance Collectable Adt where
  collect = collectAdt

instance Collectable Signature where
  collect = collectSignature

instance Collectable Declaration where
  collect = collectDeclaration

instance Collectable Constructor where
  collect = collectConstructor

instance Collectable Program where
  collect = collectProgram

instance (Collectable a) => Collectable [a] where
  collect = mapM_ collect
