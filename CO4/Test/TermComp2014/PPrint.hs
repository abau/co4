module CO4.Test.TermComp2014.PPrint
where

import           Data.Char (chr)
import           Data.List (intercalate,intersperse)
import qualified Data.Map as M
import           Unsafe.Coerce
import           CO4.Util (fromBinary)
import           CO4.Test.TermComp2014.Standalone 
  (Domain,Symbol,Trs(..),Rule(..),Term(..),Model,Precedence)

pprintTrs :: Int -> Trs () -> String
pprintTrs n = pprintTrs' n $ const ""

pprintLabeledTrs :: Int -> Trs [Domain] -> String
pprintLabeledTrs n = pprintTrs' n pprintLabel

pprintTrs' :: Int -> (a -> String) -> Trs a -> String
pprintTrs' n f (Trs rules) = unlines $ map goRule rules
  where
    goRule (Rule l r) = concat [ goTerm l, " -> ", goTerm r ]

    goTerm (Var v) = pprintSymbol v

    goTerm (Node s l args) = 
      concat [ pprintSymbol s, "^", f l ," (", intercalate ", " (map goTerm args), ")" ]

pprintValue :: Domain -> String
pprintValue = show . fromBinary

pprintSymbol :: Symbol -> String
pprintSymbol = return . chr . fromBinary

pprintModel :: Model -> String
pprintModel = unlines . intersperse "" . map pprintInterpretation
  where
    pprintInterpretation (s,i) = unlines $ map (pprintMapping $ pprintSymbol s) i
      where
        pprintMapping s (xs, y) = 
          concat [ s, " ", intercalate " " (map pprintValue xs), " |-> ", pprintValue y ]

pprintLabel :: [Domain] -> String
pprintLabel vs = "[" ++ (intercalate ", " $ map pprintValue vs) ++ "]"

pprintPrecedence :: Precedence -> String
pprintPrecedence = unlines . map go
  where
    go ((s,l),n) = pprintSymbol s ++ "^" ++ pprintLabel l ++ " |-> " ++ show n
