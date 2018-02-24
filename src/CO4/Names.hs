{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- |Namelike definitions
module CO4.Names 
  ( Namelike (..), convertName, funName, listName, nilName, consName
  , tupleTypeName, tupleDataName, maybeName, eitherName, orderingName
  , natName, natTypeName, intName, boolName, unitName
  , mainName, deprecatedMainName, toValidTypeIdentifier, toValidDataIdentifier
  )
where

import           CO4.Language (Name(..),UntypedName(..))
import qualified Language.Haskell.TH as TH

class Namelike a where
  readName    :: String -> a
  fromName    :: a -> String

  mapName     :: (String -> String) -> a -> a
  mapName f   =  readName . f . fromName

  untypedName :: a -> UntypedName
  untypedName =  UntypedName . fromName

  name        :: a -> Name
  name        =  NUntyped . fromName

instance Namelike String where
  readName    = id
  fromName    = id

instance Namelike UntypedName where
  readName                    = UntypedName
  fromName  (UntypedName n)   = n

instance Namelike Name where
  readName                 = NUntyped
  fromName (NUntyped n)    = n
  fromName (NTyped n _)    = n
  mapName f (NUntyped n)   = NUntyped $ f n
  mapName f (NTyped n s)   = NTyped (f n) s
  name                     = id

instance Namelike TH.Name where
  readName = TH.mkName
  fromName = show

convertName :: (Namelike n,Namelike m) => n -> m
convertName = readName . fromName

funName :: Namelike n => n
funName = convertName $ TH.nameBase ''(->)

listName :: Namelike n => n
listName = convertName $ TH.nameBase ''[]

nilName :: Namelike n => n
nilName = convertName $ TH.nameBase '[]

consName :: Namelike n => n
consName = convertName $ TH.nameBase '(:)

maybeName :: Namelike n => n
maybeName = convertName $ TH.nameBase ''Maybe

eitherName :: Namelike n => n
eitherName = convertName $ TH.nameBase ''Either

orderingName :: Namelike n => n
orderingName = convertName $ TH.nameBase ''Ordering

tupleDataName :: Namelike n => Int -> n
tupleDataName = convertName . TH.nameBase . TH.tupleDataName

tupleTypeName :: Namelike n => Int -> n
tupleTypeName = convertName . TH.nameBase . TH.tupleTypeName

natTypeName :: Namelike n => n
natTypeName = readName "Nat"

natName :: Namelike n => n
natName = readName "nat"

intName :: Namelike n => n
intName = convertName $ TH.nameBase ''Int

boolName :: Namelike n => n
boolName = convertName $ TH.nameBase ''Bool

unitName :: Namelike n => n
unitName = convertName $ TH.nameBase ''()

mainName :: Namelike n => n
mainName = readName "constraint"

deprecatedMainName :: Namelike n => n
deprecatedMainName = readName "main"

toValidTypeIdentifier :: (Eq n, Namelike n) => n -> n
toValidTypeIdentifier name = case name of
  n | n == listName        -> readName "List"
  n | n == tupleDataName 2 -> readName "Tuple2"
  n | n == tupleDataName 3 -> readName "Tuple3"
  n | n == tupleDataName 4 -> readName "Tuple4"
  n | n == tupleDataName 5 -> readName "Tuple5"
  n | n == tupleTypeName 2 -> readName "Tuple2"
  n | n == tupleTypeName 3 -> readName "Tuple3"
  n | n == tupleTypeName 4 -> readName "Tuple4"
  n | n == tupleTypeName 5 -> readName "Tuple5"
  n | n == unitName        -> readName "Unit"
  n                        -> n

toValidDataIdentifier :: (Eq n, Namelike n) => n -> n
toValidDataIdentifier name = case name of
  n | n == nilName         -> readName "Nil"
  n | n == consName        -> readName "Cons"
  n | n == tupleDataName 2 -> readName "Tuple2"
  n | n == tupleDataName 3 -> readName "Tuple3"
  n | n == tupleDataName 4 -> readName "Tuple4"
  n | n == tupleDataName 5 -> readName "Tuple5"
  n | n == tupleTypeName 2 -> readName "Tuple2"
  n | n == tupleTypeName 3 -> readName "Tuple3"
  n | n == tupleTypeName 4 -> readName "Tuple4"
  n | n == tupleTypeName 5 -> readName "Tuple5"
  n | n == unitName        -> readName "Unit"
  n                        -> n
