-- |Haskell-Src-Exts front end. This actually transforms Haskell-Src-Exts's AST
-- to Template-Haskell's AST and uses the according front end.
module CO4.Frontend.HaskellSrcExts
  (module CO4.Frontend)
where

import qualified Language.Haskell.Exts as HE
import qualified Language.Haskell.Meta as HM
import           Debug.Trace (trace)
import           CO4.Frontend
import           CO4.Frontend.TH ()

instance ProgramFrontend HE.Module where
  parseProgram (HE.Module _ _ [] Nothing _ imports decs) =
    if null imports then p
    else trace "Frontend.HaskellSrcExts (Warning): Import declarations will be deleted" p
    where 
      p = parseProgram $ HM.toDecs decs

  parseProgram _ = error $ "Frontend.HaskellSrcExts.parseProgram: no pragmas or warning texts allowed"

  parsePreprocessedProgram (HE.Module _ _ [] Nothing _ imports decs) =
    if null imports then p
    else trace "Frontend.HaskellSrcExts (Warning): Import declarations will be deleted" p
    where 
      p = parsePreprocessedProgram $ HM.toDecs decs

  parsePreprocessedProgram _ = error $ "Frontend.HaskellSrcExts.parsePreprocessedProgram: no pragmas or warning texts allowed"

instance ExpressionFrontend HE.Exp where
  parseExpression             = parseExpression . HM.toExp
  parsePreprocessedExpression = parsePreprocessedExpression . HM.toExp
