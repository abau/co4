{-# LANGUAGE FlexibleInstances #-}
module CO4.Names 
where

import CO4.Language (Name(..),UntypedName(..),Scheme)

class Namelike a where
  readName    :: String -> a
  fromName    :: a -> String
  mapName     :: (String -> String) -> a -> a
  untypedName :: a -> UntypedName
  name        :: a -> Name

instance Namelike String where
  readName    = id
  fromName    = id
  mapName f n = f n
  untypedName = UntypedName
  name n      = name $ untypedName n

instance Namelike UntypedName where
  readName                    = UntypedName
  fromName  (UntypedName n)   = n
  mapName f (UntypedName n)   = UntypedName $ f n
  untypedName                 = id
  name (UntypedName n)        = NUntyped n

instance Namelike Name where
  readName                 = NUntyped
  fromName (NUntyped n)    = n
  fromName (NTyped n _)    = n
  mapName f (NUntyped n)   = NUntyped $ f n
  mapName f (NTyped n s)   = NTyped (f n) s
  untypedName (NUntyped n) = UntypedName n
  untypedName (NTyped n _) = UntypedName n
  name                     = id

nTyped :: Name -> Scheme -> Name
nTyped n = NTyped $ fromName n

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
unitType   = UntypedName "Unit"

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
unitCon  = NUntyped "Unit"

tupleCon :: Int -> Name
tupleCon 1 = error "tupleCon: 1"
tupleCon n = NUntyped $ "Tuple" ++ show n

isTupleCon :: Name -> Bool
isTupleCon name = any (== name) $ map tupleCon [2..6]
