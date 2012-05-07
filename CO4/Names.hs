module CO4.Names 
where

import CO4.Language (Name(..),Scheme)

-- |Builds a typed name using a name and a scheme
typedName :: Name -> Scheme -> Name
typedName (Name n) s        = TypedName n s
typedName (TypedName n _) s = TypedName n s

-- |Builds the untyped name of a name
untypedName :: Name -> Name
untypedName (TypedName n _) = Name n 
untypedName name            = name

-- |Checks whether a name is typed
isTypedName :: Name -> Bool
isTypedName (TypedName {}) = True
isTypedName _              = False

-- |Gets the string componenet of a name
fromName :: Name -> String
fromName (Name n)        = n
fromName (TypedName n _) = n

-- Type names
listType, boolType, funType, intType, charType, doubleType :: Name
listType   = Name "List"
boolType   = Name "Bool"
funType    = Name "->"
intType    = Name "Int"
charType   = Name "Char"
doubleType = Name "Double"

tupleType :: Int -> Name
tupleType = tupleCon

-- Constructor names
consCon, nilCon, trueCon, falseCon :: Name
consCon  = Name "Cons"
nilCon   = Name "Nil"
trueCon  = Name "True"
falseCon = Name "False"

tupleCon :: Int -> Name
tupleCon 1 = error "tupleCon: 1"
tupleCon n = Name $ "Tuple" ++ show n

