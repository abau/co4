{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.UndefinedValues
  (undefinedValues)
where

import CO4.Algorithms.Instantiator
import CO4.Language
import CO4.Unique (MonadUnique,newName)
import CO4.Algorithms.UndefinedValues.Data 
  (optionalValueTypeName,definedConstructorName,undefinedConstructorName)
import CO4.Util (isInt)
import CO4.Names (fromName,natName)

newtype UndefinedValues u a = UndefinedValues { runUndefinedValues :: u a }
  deriving (Functor, Applicative, Monad, MonadUnique)

instance MonadUnique u => MonadInstantiator (UndefinedValues u) where

  instantiateCon (ECon c) = return $
    if isInt (fromName c) then ECon c
    else EApp (ECon definedConstructorName) [ECon c]

  instantiateUndefined = return $ ECon undefinedConstructorName

  instantiateApp (EApp e es) = do
    es' <- instantiate es
    case e of
      ECon _                -> return $ EApp (ECon definedConstructorName) [EApp e es']
      EVar v | v == natName -> return $ EApp (ECon definedConstructorName) [EApp e es']
      EVar _                -> do e' <- instantiate e
                                  return $ EApp e' es'

  instantiateCase (ECase e ms) = do
    ms'     <- instantiate ms
    n       <- newName ""
    (outer, inner) <- 
      case e of
        EApp (EVar f) [ECon l, ECon c, me] | fromName f == "markedDiscriminant" -> 
          do me' <- instantiate me
             return (me', EApp (EVar f) [ECon l, ECon c, EVar n])

        _ -> do e' <- instantiate e
                return (e', EVar n)

    return $ ECase outer
           [ Match (PCon definedConstructorName [PVar n]) $ ECase inner ms'
           , Match (PCon undefinedConstructorName [])     $ ECon $ undefinedConstructorName
           ]

  instantiateConstructor (CCon name types) = return $ CCon name 
                                                    $ map goType types
    where
      goType (TVar v) = TVar v
      goType (TCon c ts) = TCon optionalValueTypeName [TCon c $ map goType ts]

undefinedValues :: MonadUnique u => Program -> u Program
undefinedValues = runUndefinedValues . instantiate
