import           System.Console.GetOpt
import           System.Environment (getArgs)
import           Control.Monad (void,when)
import           Data.List (intercalate)
import qualified Language.Haskell.Exts as HE
import           CO4

main :: IO ()
main = do
  (configs, file) <- parseArgs
  input           <- readFile file
  case HE.parseModule input of
    HE.ParseFailed loc msg -> error $ "Parse error at '" ++ show loc ++ "': " ++ msg
    HE.ParseOk p           -> void $ compile p configs

configurations :: [OptDescr (Configs -> Configs)]
configurations = 
  [ Option ['v'] ["verbose"] (NoArg $ (:) Verbose) "Log verbosely"
  , Option ['d'] ["degree"]
      (ReqArg (\d -> (:) (Degree $ read d)) "DEGREE") "Degree of Raml's analysis"
  , Option [] ["degree-loop"]
      (ReqArg (\d -> (:) (DegreeLoop $ read d)) "DEGREE") "Raml analyses up to degree DEGREE (inclusive)"
  , Option ['m'] ["metric"]
      (ReqArg (\m -> (:) (Metric m)) "METRIC") ("Metric of Raml's analysis: " ++ show metrics)
  , Option [] ["no-raml"] (NoArg $ (:) NoRaml) "No Raml analysis"
  , Option [] ["no-satchmo"] (NoArg $ (:) NoSatchmo) "No Satchmo code generation"
  , Option [] ["dump-after"]
      (ReqArg (\s -> (:) (parseDumpAfter s)) "STAGE[.FILE]") ("Dump code. STAGE=" ++ show stageNames)
  , Option [] ["dump-all"]
      (ReqArg (\fp -> (:) (DumpAll fp)) "FILE") ("Dump code of all intermediate stages")
  , Option [] ["instantiation-depth"]
      (ReqArg (\i -> (:) (InstantiationDepth $ read i)) "DEPTH") ("Maximum instantiation depth (default: " ++ show defaultInstantiationDepth ++ ")")
  , Option ['i'] ["import-prelude"] (NoArg $ (:) ImportPrelude) "Import prelude"
  ]

  where
    parseDumpAfter string = case break (== '.') string of
      (stage,[])   -> checkStage stage $ DumpAfter stage ""
      (stage,rest) -> checkStage stage $ DumpAfter stage $ tail rest

      where checkStage s = if s `elem` stageNames then id 
                           else error $ concat [ "Unknown stage '",s,"'. "
                                               , "Available stages: ", show stageNames
                                               ]
parseArgs :: IO (Configs, FilePath)
parseArgs = do
  args <- getArgs
  case getOpt RequireOrder configurations args of
    (o,fp:n,[]) ->
      do when (not $ null n) $ putStrLn $ "Omitting arguments " ++ (intercalate "," n)
         return $ (foldr ($) [] o, fp)
    (_,[],errors) -> error $ concat errors ++ usageInfo "No input file" configurations
    (_,_,errors)  -> error $ concat errors ++ usageInfo "" configurations
