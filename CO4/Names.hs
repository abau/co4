{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- |Namelike definitions
module CO4.Names 
  (Namelike (..), funName, convertName)
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

funName :: Namelike n => n
funName = readName "->"

convertName :: (Namelike n,Namelike m) => n -> m
convertName = readName . fromName
