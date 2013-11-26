module CO4.TypesUtil
  ( splitScheme, quantifiedNames, typeOfScheme, isFunType, splitFunType
  , argumentTypes, resultType, functionType, fromSType, typeOfAdt, schemeOfName)
where

import           CO4.Language
import           CO4.Names (funName)
import           CO4.PPrint (PPrint (..))

-- |Returns the quantified names and the embedded type of a scheme 
splitScheme :: Scheme -> ([UntypedName],Type)
splitScheme scheme = case scheme of
  SType t     -> ([],t)
  SForall n s -> (\(ns,t) -> (n:ns,t)) $ splitScheme s

-- |Returns the quantified names of a scheme 
quantifiedNames :: Scheme -> [UntypedName]
quantifiedNames = fst . splitScheme

-- |Returns the embedded type of a scheme 
typeOfScheme :: Scheme -> Type
typeOfScheme = snd . splitScheme

-- |Returns whether top level type constructor is a function constructor
isFunType :: Type -> Bool
isFunType scheme = case scheme of
  TCon c [_,_] -> c == funName
  _            -> False

-- |Splits a function type @a -> b -> c@ in @([a,b],c)@
splitFunType :: Type -> ([Type],Type)
splitFunType type_ = case type_ of
  (TCon c [a,b]) | c == funName -> let (parametersT,resultT) = splitFunType b
                                   in 
                                     (a : parametersT, resultT)
  t                             -> ([],t)

-- |Returns the argument types of a function type
argumentTypes :: Type -> [Type]
argumentTypes = fst . splitFunType

-- |Returns the result type of a function type
resultType :: Type -> Type
resultType = snd . splitFunType

-- |Builds a function scheme from a list of parameter types and a result type
functionType :: [Type] -> Type -> Type
functionType = flip $ foldr (\a b -> TCon funName [a,b]) 

fromSType :: Scheme -> Type
fromSType (SType t) = t
fromSType scheme = error $ "TypesUtil: fromSType (" ++ show (pprint scheme) ++ ")"

-- |Returns the algebraic data type in terms of a @Type@ instance
typeOfAdt :: Adt -> Type
typeOfAdt adt = TCon (adtName adt) $ map TVar $ adtTypeVariables adt

-- |Returns the scheme of a typed name
schemeOfName :: Name -> Scheme
schemeOfName (NTyped _ s) = s
schemeOfName (NUntyped n) = error $ "TypesUtil.schemeOfName: " ++ n ++ " is untyped"
