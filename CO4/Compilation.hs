module CO4.Compilation
  (Config(..), Configs, compile)
where

import           Control.Monad.Reader hiding (liftIO)
import           Control.Applicative ((<$>))
import qualified Raml.RAMLTypes as RamlT
import qualified Raml.SimpleTypes as RamlST
import qualified Raml.Ast as RamlA
import qualified Raml.PPrint as RamlPP
import qualified Language.Haskell.TH as TH
import           CO4.Language (Program)
import           CO4.Unique (Unique,UniqueT,runUniqueT,mapUnique)
import           CO4.Frontend
import           CO4.Backend
import           CO4.Backend.Raml
import           CO4.Backend.SatchmoPreprocess
import           CO4.Algorithms.Globalize
import           CO4.Algorithms.UniqueNames
import           CO4.Algorithms.Monadify
import           CO4.Algorithms.HindleyMilner (schemes,prelude)
import           CO4.Algorithms.Instantiation (instantiation)

data Config  = Verbose
             | Degree Int
             | NoRaml
             | DumpRaml FilePath
             deriving (Eq)

type Configs      = [Config]
type Configurable = ReaderT Configs (UniqueT IO)

compile :: (ProgramFrontend a) => a -> Configs -> IO [TH.Dec]
compile a configs = do
  runUniqueT $ flip runReaderT configs $ do
    uniqueProgram <- liftUnique $ parseProgram a >>= uniqueNames
    logWhenVerbose $ unlines [ "## Intermediate language ##############"
                             , displayProgram uniqueProgram]
    satchmo <- compileToSatchmo uniqueProgram

    whenNot' NoRaml $ do 
      ramlP <- compileToRaml uniqueProgram 
      case isDumpRaml configs of
        Nothing   -> return ()
        Just file -> lift $ lift $ writeFile file $ show $ RamlPP.pprint ramlP
      analyseRaml ramlP

    return satchmo

  where
    analyseRaml (decs,main) =
      case RamlST.analyseDecs decs of
        Left msg    -> error $ "Raml.analyseDecs: " ++ msg
        Right decs' -> case RamlST.typeProgram decs' main of
            Left msg                     -> error $ "Raml.typeProgram: " ++ msg
            Right (typedDecs, typedMain) -> do
              logWhenVerbose "Raml is analysing ..."
              c <- liftIO $ RamlA.analyseAST typedMain typedDecs RamlT.evalSteps 
                          $ degree configs
              when (not c) $ error "Raml says: No!"

compileToRaml :: Program -> Configurable RamlT.Program
compileToRaml p = do        
  (ramlP) <- liftUnique $ globalize p
                       >>= \p'      -> schemes prelude p'
                       >>= instantiation

  logWhenVerbose $ unlines [ "## Raml ###############################"
                           , displayProgram ramlP]

  liftUnique $ displayPreprocessedRamlProgram ramlP

compileToSatchmo :: Program -> Configurable [TH.Dec]
compileToSatchmo p = do
  satchmoP <- liftUnique $ monadify p >>= return . preprocessSatchmoProgram

  logWhenVerbose $ unlines [ "## Satchmo ############################"
                           , displayProgram satchmoP]
  return $ displayProgram satchmoP

liftUnique :: Unique a -> Configurable a
liftUnique = lift . mapUnique

liftIO :: IO a -> Configurable a
liftIO = lift . lift

logWhenVerbose :: String -> Configurable ()
logWhenVerbose msg = when' Verbose $ liftIO $ putStrLn msg

is :: Config -> Configurable Bool
is c = elem c <$> ask

when' :: Config -> Configurable () -> Configurable ()
when' c doThis = is c >>= \b -> if b then doThis else return ()

whenNot' :: Config -> Configurable () -> Configurable ()
whenNot' c doThis = is c >>= \b -> if not b then doThis else return ()

degree :: Configs -> Int
degree cs = case cs of
  (Degree d):_ -> d
  []           -> error "Compilation: No degree provided"
  _            -> degree $ tail cs

isDumpRaml :: Configs -> Maybe FilePath
isDumpRaml cs = case cs of
  (DumpRaml fp):_ -> Just fp
  []              -> Nothing
  _               -> isDumpRaml $ tail cs
