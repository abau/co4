{-# LANGUAGE FlexibleInstances #-}
-- |Pretty printer 
module CO4.PPrint
  (PPrint (..))
where

import Text.PrettyPrint 
import CO4.Language
import CO4.Names (funType)

class PPrint a where
  pprint :: a -> Doc

-- |@parensHsep f xs@ is a list version of @<+>@ where each element @x@ of @xs@ is
-- wrapped in parens if @f x@ holds.
parensHsep :: PPrint a => (a -> Bool) -> [a] -> Doc
parensHsep f = hsep . map (\x -> ( if f x then parens else id ) $ pprint x)

instance PPrint Name where
  pprint (Name n)        = text n
  pprint (TypedName n _) = text n
  --pprint (TypedName n t) = text n <+> (brackets $ pprint t)

instance PPrint Literal where
  pprint (LInt l) | l < 0    = parens $ int $ fromIntegral l
  pprint (LInt l)            =          int $ fromIntegral l
  pprint (LChar l)           = char l
  pprint (LDouble l) | l < 0 = parens $ double l
  pprint (LDouble l)         =          double l

instance PPrint Pattern where
  pprint (PVar name)    = pprint name
  pprint (PLit lit)     = pprint lit
  pprint (PCon name ps) = 
    let isSimple (PVar _)    = True
        isSimple (PCon _ []) = True
        isSimple _           = False
    in 
      pprint name <+> parensHsep (not . isSimple) ps

instance PPrint Match where
  pprint (Match pat e) = hsep [pprint pat, text "->", pprint e, text ";"]

instance PPrint Expression where
  pprint (EVar name)   = pprint name
  pprint (ECon name)   = pprint name
  pprint (ELit lit)    = pprint lit
  pprint (EApp f args) = parensHsep (not . isSimple) $ f : args
    where isSimple (EVar {})  = True
          isSimple (ECon {})  = True
          isSimple (ELit {})  = True
          isSimple (ETApp {}) = True
          isSimple _          = False

  pprint (ETApp f subst) = parens $ (pprint f) <+> 
                              (   char '<'
                               <> (hcat $ punctuate (char ',') $ map pprint subst)
                               <> char '>')

  pprint (ELam  ns e) = hsep $ (char '\\' : map pprint ns) ++ [text "->", pprint e]
  pprint (ETLam ns e) = brackets (hsep $ (text "/\\" : map pprint ns) ++ [char '.'])
                          <+> pprint e

  pprint (ECase e matches) = 
    vcat $ [hsep [text "case", pprint e, text "of {"]]
        ++ (map (nest 2 . pprint) matches)
        ++ [text "}"]

  pprint (ELet n e1 e2) =
    vcat [ hsep [ text "let", pprint n, text "=", pprint e1]
         , text "in"
         , nest 2 $ pprint e2
         ]

instance PPrint Type where
  pprint (TVar name)   = pprint name
  pprint (TCon f args) = 
    case args of
      [a,b] | f == funType -> parensHsep (not . isSimple) [a,TVar f,b]
      _                    -> parensHsep (not . isSimple) $ (TVar f) : args
    where isSimple (TVar _)    = True
          isSimple (TCon _ []) = True
          isSimple _           = False

instance PPrint Scheme where
  pprint (SType t)     = pprint t
  pprint (SForall n s) = hsep [ text "forall", pprint n, text ".", pprint s ]

instance PPrint Declaration where
  pprint (DBind (TypedName name t) e) = 
    hsep [ text name, text "::", pprint t, text "="] $$ (nest 2 $ pprint e <+> (text ";"))

  pprint (DBind name e)  = hsep [ pprint name, text "="] $$ (nest 2 $ pprint e <+> (text ";"))

instance PPrint Program where 
  pprint decs = vcat $ map pprint decs

instance PPrint (Name,Type) where
  pprint (n,t) = hsep [pprint n, text "=", pprint t]
