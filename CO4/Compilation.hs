{-# LANGUAGE ScopedTypeVariables #-}
module CO4.Compilation
  (Config(..), Configs, compile, metrics)
where

import           Prelude hiding (catch)
import           Control.Exception (catch,ErrorCall(..))
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
import           CO4.Backend.TH ()
import           CO4.Backend.SatchmoPreprocess
import           CO4.Algorithms.Globalize
import           CO4.Algorithms.UniqueNames
import           CO4.Algorithms.Monadify
import           CO4.Algorithms.HindleyMilner (schemes,prelude)
import           CO4.Algorithms.Instantiation (instantiation)
import           CO4.Algorithms.Eitherize (eitherize)

data Config  = Verbose
             | Degree Int
             | DegreeLoop Int
             | Metric String
             | NoRaml
             | NoSatchmo
             | DumpRaml FilePath
             | DumpSatchmo FilePath
             deriving (Eq)

type Configs      = [Config]
type Configurable = ReaderT Configs (UniqueT IO)

compile :: (ProgramFrontend a) => a -> Configs -> IO [TH.Dec]
compile a configs = do
  runUniqueT $ flip runReaderT configs $ do
    uniqueProgram <- liftUnique $ parseProgram a >>= uniqueNames
    logWhenVerbose $ unlines [ "## Parsed #############################"
                             , displayProgram uniqueProgram]
    
    whenNot' NoRaml $ do 
      ramlP <- compileToRaml uniqueProgram 
      case isDumpRaml configs of
        Nothing   -> return ()
        Just file -> lift $ lift $ writeFile file $ show $ RamlPP.pprint ramlP
      analyseRaml ramlP $ degree configs

    noSatchmo <- is NoSatchmo
    if noSatchmo 
     then return []
     else do satchmo <- compileToSatchmo uniqueProgram
             case isDumpSatchmo configs of
                Nothing   -> return ()
                Just file -> lift $ lift $ writeFile file $ show $ TH.ppr satchmo
             return satchmo
  where
    m = metric configs

    analyseRaml (decs,main) d =
      case RamlST.analyseDecs decs of
        Left msg    -> error $ "Raml.analyseDecs: " ++ msg
        Right decs' -> case RamlST.typeProgram decs' main of
          Left msg                     -> error $ "Raml.typeProgram: " ++ msg
          Right (typedDecs, typedMain) -> do
            logWhenVerbose $ "Raml is analysing using degree " ++ show d ++ " ..."
            c <- liftIO $ ( do r <- RamlA.analyseAST typedMain typedDecs m d
                               seq r $ return r )
                    `catch`
                    (\(ErrorCall msg) -> do putStrLn $ "Raml aborted: " ++ msg
                                            return False
                    )
            if c
             then do
               logWhenVerbose $ "Raml successfully finished analysis of degree " ++ show d
               return ()
             else case isDegreeLoop configs of
                    Nothing  -> error $ "Raml can not infer degree " ++ show d
                    Just max | d < max -> analyseRaml (decs,main) (d + 1)
                    Just max -> error $ "Raml can not infer degree up to " ++ show max

compileToRaml :: Program -> Configurable RamlT.Program
compileToRaml p = do        
  (ramlP) <- liftUnique $ globalize p
                       >>= \p'      -> schemes prelude p'
--                       >>= instantiation

  logWhenVerbose $ unlines [ "## Raml ###############################"
                           , displayProgram ramlP]

  liftUnique $ displayPreprocessedRamlProgram ramlP

compileToSatchmo :: Program -> Configurable [TH.Dec]
compileToSatchmo p = do
  satchmoP <- liftUnique $ {-monadify-} return p 
                >>= eitherize
                >>= monadify
                >>= return . preprocessSatchmoProgram

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

metrics :: [String]
metrics = [ "heap-space", "eval-steps", "overflow", "cost-free" ]
metric :: Configs -> RamlT.ResourceMetric
metric cs = case cs of
  (Metric "heap-space"):_ -> RamlT.heapSpace
  (Metric "eval-steps"):_ -> RamlT.evalSteps
  (Metric "overflow"):_   -> RamlT.overflow
  (Metric "cost-free"):_  -> RamlT.costFree
  (Metric m):_            -> error $ concat 
                              [ "Compilation: Unknown metric '", m, "'. "
                              , "Available metrics: ", show metrics 
                              ]
  []                      -> error "Compilation: No metric provided"
  _                       -> metric $ tail cs

isDumpRaml :: Configs -> Maybe FilePath
isDumpRaml cs = case cs of
  (DumpRaml fp):_ -> Just fp
  []              -> Nothing
  _               -> isDumpRaml $ tail cs

isDumpSatchmo :: Configs -> Maybe FilePath
isDumpSatchmo cs = case cs of
  (DumpSatchmo fp):_ -> Just fp
  []                 -> Nothing
  _                  -> isDumpSatchmo $ tail cs

isDegreeLoop :: Configs -> Maybe Int
isDegreeLoop cs = case cs of
  (DegreeLoop d):_ -> Just d
  []               -> Nothing
  _                -> isDegreeLoop $ tail cs
