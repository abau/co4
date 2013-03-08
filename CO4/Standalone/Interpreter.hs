module CO4.Standalone.Interpreter
  (interpret)
where

import           Language.Haskell.Interpreter hiding (interpret)
import qualified Language.Haskell.Interpreter as I
import           Data.Typeable (Typeable)

interpret :: Typeable b => FilePath -> String -> IO b
interpret filePath expression = 
  runInterpreter (interpreter filePath expression) >>= \case 
    Right result          -> result 
    Left (UnknownError e) -> error $ "Standalone.Interpreter: unknown error: " ++ e
    Left (NotAllowed   e) -> error $ "Standalone.Interpreter: not allowed: " ++ e
    Left (GhcException e) -> error $ "Standalone.Interpreter: GHC exception: " ++ e
    Left (WontCompile es) -> error $ "Standalone.Interpreter: wont compile:\n" 
                                  ++ (unlines $ map errMsg es)

interpreter :: Typeable b => FilePath -> String -> Interpreter b
interpreter filePath expression = do
  set [ languageExtensions      := [ UnknownExtension "ImplicitPrelude" 
                                   , UnknownExtension "LambdaCase" 
                                   ]
      , installedModulesInScope := True
      ]
  loadModules [filePath]
  setImports  ["Prelude"]
  I.interpret expression as 
