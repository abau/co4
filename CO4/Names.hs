{-# LANGUAGE FlexibleInstances #-}
module CO4.Names 
where

import CO4.Language (Name(..),UntypedName(..),TypedName(..),Scheme)

class Namelike a where
  readName    :: String -> a
  fromName    :: a -> String
  mapName     :: (String -> String) -> a -> a
  typedName   :: a -> Scheme -> TypedName
  untypedName :: a -> UntypedName
  name        :: a -> Name

instance Namelike String where
  readName    = id
  fromName    = id
  mapName f n = f n
  typedName   = TypedName
  untypedName = UntypedName
  name n      = name $ untypedName n

instance Namelike UntypedName where
  readName                    = UntypedName
  fromName  (UntypedName n)   = n
  mapName f (UntypedName n)   = UntypedName $ f n
  typedName (UntypedName n) s = TypedName n s
  untypedName                 = id
  name (UntypedName n)        = NUntyped n

instance Namelike TypedName where
  readName s                  = error $ "Can not read TypedName from string '" ++ s ++ "'"
  fromName  (TypedName n _)   = n
  mapName f (TypedName n s)   = TypedName (f n) s
  typedName (TypedName n _) s = TypedName n s
  untypedName (TypedName n _) = UntypedName n
  name (TypedName n s)        = NTyped n s

instance Namelike Name where
  readName                 = NUntyped
  fromName (NUntyped n)    = n
  fromName (NTyped n _)    = n
  mapName f (NUntyped n)   = NUntyped $ f n
  mapName f (NTyped n s)   = NTyped (f n) s
  typedName (NUntyped n) s = TypedName n s
  typedName (NTyped n _) s = TypedName n s
  untypedName (NUntyped n) = UntypedName n
  untypedName (NTyped n _) = UntypedName n
  name                     = id

nTyped :: Name -> Scheme -> Name
nTyped n = name . typedName n

nUntyped :: Name -> Name
nUntyped = name . untypedName

-- Type names
listType, boolType, funType, intType, charType, doubleType :: UntypedName
listType   = UntypedName "List"
boolType   = UntypedName "Bool"
funType    = UntypedName "->"
intType    = UntypedName "Int"
charType   = UntypedName "Char"
doubleType = UntypedName "Double"

tupleType :: Int -> UntypedName
tupleType = untypedName . tupleCon

isTupleType :: UntypedName -> Bool
isTupleType = isTupleCon . name

-- Constructor names
consCon, nilCon, trueCon, falseCon :: Name
consCon  = NUntyped "Cons"
nilCon   = NUntyped "Nil"
trueCon  = NUntyped "True"
falseCon = NUntyped "False"

tupleCon :: Int -> Name
tupleCon 1 = error "tupleCon: 1"
tupleCon n = NUntyped $ "Tuple" ++ show n

isTupleCon :: Name -> Bool
isTupleCon name = any (== name) $ map tupleCon [2..6]
