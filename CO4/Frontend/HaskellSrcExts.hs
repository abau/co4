-- |Haskell-Src-Exts front end. This actually transforms Haskell-Src-Exts's AST
-- to Template-Haskell's AST and uses the according front end.
module CO4.Frontend.HaskellSrcExts
  ()
where

import qualified Language.Haskell.Exts as HE
import qualified Language.Haskell.Meta as HM
import           CO4.Frontend
import           CO4.Frontend.TH ()

instance ProgramFrontend HE.Module where
  parseProgram (HE.Module _ _ [] Nothing _ [] decs) =
    parseProgram $ HM.toDecs decs

  parseProgram _ = error $ "Frontend.HaskellSrcExts.parseProgram: no pragmas, warning texts or import declarations allowed"

  parsePreprocessedProgram (HE.Module _ _ [] Nothing _ [] decs) =
    parsePreprocessedProgram $ HM.toDecs decs

  parsePreprocessedProgram _ = error $ "Frontend.HaskellSrcExts.parsePreprocessedProgram: no pragmas, warning texts or import declarations allowed"
