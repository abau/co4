{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CO4.Test.Maybe
where

import           Prelude hiding (Bool(..),and,not,(&&),(||))
import           Control.Applicative ((<$>))
import qualified Language.Haskell.TH as TH
import           Satchmo.Boolean (assert)
import           Satchmo.Code (Decode,decode)
import           Satchmo.SAT.Mini (SAT,solve)
import           CO4
import           CO4.EncodedAdt

$([d|
      --data Vielleicht a =  Nichts | Genau a
      data Farbe = Rot | Blau
      data Aussage = Wahr | Falsch

      und x y = case x of Wahr  -> case y of Wahr  -> Wahr
                                             Falsch -> Falsch
                          Falsch -> Falsch

      {-
      genau vielleicht = case vielleicht of
        Genau a -> a
        Nichts  -> undefined
      -}

      istRot farbe = case farbe of
          Rot -> Wahr
          Blau -> Falsch

      --nichts = Nichts

      main x = istRot x

   |] >>= \p -> TH.runIO $ compile p [ Verbose, NoRaml
                                     , DumpAll "" 
                                     ]
  )
{- 
invMain :: Param -> IO ()
invMain param = do
  mResult <- solve ( do u <- unknownVielleicht
                        a <- encMain param u v
                        assert [flag a]
                        return ( decode [u,v] )
                 ) :: IO (Maybe [Fuzzy])
  putStrLn $ show mResult

deriving instance Show Fuzzy
deriving instance Show Boolean
-}
