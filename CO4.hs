module CO4
  ( module CO4.Compilation
  , Config (..), Configs, configurable
  , prelude
  , module CO4.Algorithms.Eitherize.Solve
  , module CO4.Allocator
  , allocates
  ) 
where

import           CO4.Frontend.String ()
import           CO4.Frontend.TH ()
import           CO4.Frontend.HaskellSrcExts ()
import           CO4.Compilation
import           CO4.Config (Config (..),Configs,configurable)
import           CO4.Prelude (prelude)
import           CO4.Algorithms.Eitherize.Solve 
import           CO4.Allocator
import           CO4.EncodedAdt (allocates)
