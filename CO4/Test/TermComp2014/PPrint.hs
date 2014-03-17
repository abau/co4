module CO4.Test.TermComp2014.PPrint
where

import           Data.Char (chr)
import           Data.List (intercalate,intersperse)
import qualified Data.Map as M
import           Unsafe.Coerce
import           CO4.Util (fromBinary)
import           CO4.Test.TermComp2014.Standalone 
  (Domain,Symbol,Label,Trs(..),Rule(..),Term(..),Model,Precedence,UnlabeledTrs,LabeledTrs)

pprintTrs :: UnlabeledTrs -> String
pprintTrs = pprintTrs' pprintSymbol pprintSymbol $ const ""

pprintLabeledTrs :: LabeledTrs -> String
pprintLabeledTrs = pprintTrs' pprintSymbol pprintSymbol pprintLabel

pprintTrs' :: (v -> String) -> (n -> String) -> (l -> String) -> Trs v n l -> String
pprintTrs' goV goN goL (Trs rules) = unlines $ map goRule rules
  where
    goRule (Rule l r) = concat [ goTerm l, " -> ", goTerm r ]

    goTerm (Var v) = goV v

    goTerm (Node s l args) = 
      case goL l of
        "" -> concat [ goN s          ," (", intercalate ", " (map goTerm args), ")" ]
        l' -> concat [ goN s, "^", l' ," (", intercalate ", " (map goTerm args), ")" ]

pprintValue :: Domain -> String
pprintValue = show . fromBinary

pprintSymbol :: Symbol -> String
pprintSymbol = return . chr . fromBinary

pprintModel :: Model Symbol -> String
pprintModel = unlines . intersperse "" . map pprintInterpretation
  where
    pprintInterpretation (s,i) = unlines $ map (pprintMapping $ pprintSymbol s) i
      where
        pprintMapping s (xs, y) = 
          concat [ s, " ", intercalate " " (map pprintValue xs), " |-> ", pprintValue y ]

pprintLabel :: Label -> String
pprintLabel vs = "[" ++ (intercalate ", " $ map pprintValue vs) ++ "]"

pprintPrecedence :: Precedence Symbol Label -> String
pprintPrecedence = unlines . map go
  where
    go ((s,l),n) = pprintSymbol s ++ "^" ++ pprintLabel l ++ " |-> " ++ show n
