{-# LANGUAGE TemplateHaskell #-}
module CO4.Prelude
  (prelude, parsePrelude)
where

import           Control.Monad (liftM)
import qualified Language.Haskell.TH as TH
import           CO4.Language (Declaration)
import           CO4.Frontend.TH (parseTHDeclaration)
import           CO4.Frontend.THPreprocess (eraseDerivings)

parsePrelude :: IO [Declaration]
parsePrelude = TH.runQ prelude >>= return . map parseTHDeclaration . eraseDerivings

prelude :: TH.Q [TH.Dec]
prelude = 
  [d| data Bool = False | True deriving Show
  
  |]
