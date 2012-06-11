-- |Debug-module for simple loading of most modules. This is not intended to
-- appear in release packages (no exposed module).
module CO4Debug
  ( module CO4.Language
  , module CO4.Names
  , module CO4.PPrint
  , module CO4.Unique
  , module CO4.Frontend
  , module CO4.Frontend.String
  , module CO4.Backend
  , module CO4.Backend.Raml
  , module CO4.Backend.SatchmoPreprocess
  , module CO4.Compilation
  , module CO4.Algorithms.UniqueNames
  , module CO4.Algorithms.Globalize
  , module CO4.Algorithms.Monadify
  , module CO4.Algorithms.Free
  , module CO4.Algorithms.Bound
  , module CO4.Algorithms.HindleyMilner
  , module CO4.Algorithms.TypedNames
  , module CO4.Algorithms.Instantiation
  , module CO4.Algorithms.EtaExpansion
  , module CO4.Algorithms.SaturateApplication
  , module CO4.Algorithms.Rename
  , module CO4.Algorithms.Collapse
  ) 
where

import           CO4.Language
import           CO4.Names
import           CO4.PPrint
import           CO4.Unique
import           CO4.Frontend
import           CO4.Frontend.String
import           CO4.Frontend.TH ()
import           CO4.Frontend.HaskellSrcExts ()
import           CO4.Backend
import           CO4.Backend.TH ()
import           CO4.Backend.Raml
import           CO4.Backend.SatchmoPreprocess
import           CO4.Compilation
import           CO4.Algorithms.UniqueNames
import           CO4.Algorithms.Globalize
import           CO4.Algorithms.Monadify
import           CO4.Algorithms.Free
import           CO4.Algorithms.Bound
import           CO4.Algorithms.HindleyMilner
import           CO4.Algorithms.TypedNames
import           CO4.Algorithms.Instantiation
import           CO4.Algorithms.EtaExpansion
import           CO4.Algorithms.SaturateApplication
import           CO4.Algorithms.Rename
import           CO4.Algorithms.Collapse
