{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.Monadify
  (monadify)
where

import Control.Applicative ((<$>))
import Control.Monad.Writer
import CO4.Language
import CO4.Unique
import CO4.Algorithms.Instantiator
import CO4.Names (fromName)

newtype Monadifier a = Monadifier { runMonadifier :: Unique a }
  deriving (Functor, Monad, MonadUnique)

dontMonadify :: Name -> Bool
dontMonadify n = fromName n `elem` no
  where no = [ "+","-","*","/",">",">=","==","/=","<=","<=" ]

returnExpression :: Expression -> Expression
returnExpression exp = EApp (EVar $ NUntyped "return") [exp]

bindExpression :: Expression -> Name -> Expression -> Expression
bindExpression exp name inExp = EApp (EVar $ NUntyped ">>=") [exp, ELam [name] inExp]

instance MonadInstantiator Monadifier where

  instantiateVar = return . returnExpression
  instantiateCon = return . returnExpression
  instantiateLit = return . returnExpression

  instantiateApp (EApp f args) = do
    f'    <- instantiateExpression f
    args' <- mapM instantiateExpression args
    names <- forM (f':args') $ const $ newName "bind"

    let (fName:argNames) = map EVar names
        app'             = case f of
          EVar v | dontMonadify v -> returnExpression $ EApp fName argNames
          ECon {}                 -> returnExpression $ EApp fName argNames
          _                       ->                    EApp fName argNames

    return $ foldr (\(exp,name) -> bindExpression exp name) app' $ zip (f':args') names

  instantiateLam (ELam ns exp) = do
    exp' <- instantiateExpression exp
    return $ returnExpression $ ELam ns exp' 

  instantiateCase (ECase exp ms) = do
    exp' <- instantiateExpression exp
    ms'  <- mapM instantiateMatch ms
    name <- newName "bind"
    return $ bindExpression exp' name $ ECase (EVar name) ms'
    
  instantiateLet (ELet n (ELam ns value) exp) = do
    value' <- instantiateExpression value
    exp'   <- instantiateExpression exp
    return $ ELet n (ELam ns value') exp'

  instantiateLet (ELet n value exp) = do
    value' <- instantiateExpression value
    exp'   <- instantiateExpression exp
    return $ bindExpression value' n $ exp'

  instantiateDeclaration (DBind n (ELam ns e)) = 
    (DBind n . ELam ns) <$> instantiateExpression e
  instantiateDeclaration (DBind n e) = 
    DBind n <$> instantiateExpression e

monadify :: Instantiable a => a -> Unique a
monadify = runMonadifier . instantiate
