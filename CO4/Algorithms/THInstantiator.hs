{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module CO4.Algorithms.THInstantiator
  (MonadTHInstantiator(..), THInstantiable(..), toTH)
where

import           Control.Monad (ap)
import           Control.Monad.Identity (Identity,runIdentity)
import           Text.Read (readEither)
import qualified Language.Haskell.TH as TH
import           CO4.Language
import           CO4.Names (fromName,funName,listName)
import           CO4.THUtil (toTHName,intE)

class Monad m => MonadTHInstantiator m where

  instantiateScheme :: Scheme -> m TH.Type
  instantiateScheme scheme = case scheme of
    SType t     -> instantiate t
    SForall n s -> do
      tvar <- return TH.PlainTV `ap` instantiate n
      s'   <- instantiate s
      return $ case s' of
        TH.ForallT bndrs [] t -> TH.ForallT (tvar : bndrs) [] t
        t                     -> TH.ForallT [tvar]         [] t

  instantiateType :: Type -> m TH.Type
  instantiateType type_ = case type_ of
    TVar v                         -> return TH.VarT `ap` instantiate v
    TCon c [a,b] | c == funName    -> do
      a' <- instantiate a
      b' <- instantiate b
      return $ TH.AppT (TH.AppT TH.ArrowT a') b'

    TCon c [a] | c == listName -> do
      a' <- instantiate a
      return $ TH.AppT TH.ListT a'

    TCon c as -> do
      c'  <- instantiate c
      as' <- instantiate as
      return $ foldl TH.AppT (TH.ConT c') as'

  instantiateUntypedName :: UntypedName -> m TH.Name
  instantiateUntypedName = return . toTHName

  -- |Note that calling @instantiateName@ will erase type information from
  -- typed names.
  instantiateName :: Name -> m TH.Name
  instantiateName = return . toTHName

  instantiatePattern :: Pattern -> m TH.Pat
  instantiatePattern pattern = case pattern of
    PVar v@(NTyped _ s) -> do 
      v' <- instantiate v
      return (TH.SigP (TH.VarP v')) `ap` instantiate s

    PVar v -> return TH.VarP `ap` instantiate v

    PCon c@(NTyped _ s) ps -> do
      c'  <- instantiate c
      ps' <- instantiate ps
      return (TH.SigP $ TH.ConP c' ps') `ap` instantiate s

    PCon c ps -> return TH.ConP `ap` instantiate c `ap` instantiate ps

  instantiateMatch :: Match -> m TH.Match
  instantiateMatch (Match p e) = do
    p' <- instantiate p
    e' <- instantiate e
    return $ TH.Match p' (TH.NormalB e') []

  instantiateBinding :: Binding -> m TH.Dec
  instantiateBinding (Binding n e) = do
    p'  <- instantiate $ PVar n
    e'  <- instantiate e
    return $ TH.ValD p' (TH.NormalB e') []

  instantiateVar :: Expression -> m TH.Exp
  instantiateVar expression = case expression of
    EVar v@(NTyped _ s) -> do
      v' <- instantiate v
      return (TH.SigE $ TH.VarE v') `ap` instantiate s

    EVar v -> return TH.VarE `ap` instantiate v

  instantiateCon :: Expression -> m TH.Exp
  instantiateCon expression = case expression of
    ECon c@(NTyped _ s) -> do
      c' <- instantiate c
      return (TH.SigE $ TH.ConE c') `ap` instantiate s

    ECon c -> case readEither (fromName c) of
      Left _  -> return TH.ConE `ap` instantiate c
      Right i -> return $ intE i

  instantiateApp :: Expression -> m TH.Exp
  instantiateApp (EApp f args) = do
    return (foldl TH.AppE) `ap` instantiate f `ap` instantiate args

  instantiateLam :: Expression -> m TH.Exp
  instantiateLam (ELam ns e) = do
    ns' <- mapM instantiate ns
    e'  <- instantiate e
    return $ TH.LamE (map TH.VarP ns') e'

  instantiateCase :: Expression -> m TH.Exp
  instantiateCase (ECase e ms) = 
    return TH.CaseE `ap` instantiate e `ap` instantiate ms

  instantiateLet :: Expression -> m TH.Exp
  instantiateLet (ELet bs exp) =
    return TH.LetE `ap` instantiate bs `ap` instantiate exp

  instantiateUndefined :: m TH.Exp
  instantiateUndefined = return $ TH.VarE 'undefined

  instantiateExpression :: Expression -> m TH.Exp
  instantiateExpression exp = case exp of
    EVar {}    -> instantiateVar exp
    ECon {}    -> instantiateCon exp
    EApp {}    -> instantiateApp exp
    ELam {}    -> instantiateLam exp
    ECase {}   -> instantiateCase exp
    ELet {}    -> instantiateLet exp
    EUndefined -> instantiateUndefined
    _          -> error $ "THInstantiator.instantiateExpression: can not instantiate '" ++ show exp ++ "'"

  instantiateConstructor :: Constructor -> m TH.Con
  instantiateConstructor (CCon n ts) =
    return TH.NormalC `ap` instantiate n `ap` mapM instantiateStrictType ts
    where 
      instantiateStrictType t = do 
        t' <- instantiate t
        return $ (TH.NotStrict, t')

  instantiateBind :: Declaration -> m TH.Dec
  instantiateBind (DBind b) = instantiateBinding b 

  instantiateAdt :: Adt -> m TH.Dec
  instantiateAdt (Adt name ts cons) = do
    name' <- instantiate name
    ts'   <- return (map TH.PlainTV) `ap` instantiate ts
    cons' <- instantiate cons
    return $ TH.DataD [] name' ts' cons' []

  instantiateSignature :: Signature -> m TH.Dec
  instantiateSignature (Signature name scheme) = do
    name'   <- instantiate name
    scheme' <- instantiate scheme
    return $ TH.SigD name' scheme'

  instantiateDeclaration :: Declaration -> m TH.Dec
  instantiateDeclaration decl = case decl of
    DBind {} -> instantiateBind decl
    DAdt adt -> instantiate adt
    DSig sig -> instantiate sig

  instantiateMain :: Binding -> m TH.Dec
  instantiateMain main = instantiateDeclaration $ DBind main

  instantiateProgram :: Program -> m [TH.Dec]
  instantiateProgram (Program main decls) = do
    main'  <- instantiateMain main
    decls' <- mapM instantiateDeclaration decls
    return $ main' : decls'

instance MonadTHInstantiator Identity

class THInstantiable a b where
  instantiate :: MonadTHInstantiator m => a -> m b

toTH :: (THInstantiable a b) => a -> b
toTH = runIdentity . instantiate

instance THInstantiable Name TH.Name where
  instantiate = instantiateName

instance THInstantiable UntypedName TH.Name where
  instantiate = instantiateUntypedName

instance THInstantiable Type TH.Type where
  instantiate = instantiateType

instance THInstantiable Scheme TH.Type where
  instantiate = instantiateScheme

instance THInstantiable Expression TH.Exp where
  instantiate = instantiateExpression

instance THInstantiable Pattern TH.Pat where
  instantiate = instantiatePattern

instance THInstantiable Match TH.Match where
  instantiate = instantiateMatch

instance THInstantiable Binding TH.Dec where
  instantiate = instantiateBinding

instance THInstantiable Adt TH.Dec where
  instantiate = instantiateAdt

instance THInstantiable Signature TH.Dec where
  instantiate = instantiateSignature

instance THInstantiable Declaration TH.Dec where
  instantiate = instantiateDeclaration

instance THInstantiable Constructor TH.Con where
  instantiate = instantiateConstructor

instance THInstantiable Program [TH.Dec] where
  instantiate = instantiateProgram

instance (THInstantiable a b) => THInstantiable [a] [b] where
  instantiate = mapM instantiate
