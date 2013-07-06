{-# LANGUAGE FlexibleInstances #-}
-- |Pretty printer 
module CO4.PPrint
  (PPrint (..))
where

import Text.PrettyPrint 
import CO4.Language
import CO4.Names (funName,name)

class PPrint a where
  pprint :: a -> Doc

-- |@parensHsep f xs@ is a list version of @<+>@ where each element @x@ of @xs@ is
-- wrapped in parens if @f x@ holds.
parensHsep :: PPrint a => (a -> Bool) -> [a] -> Doc
parensHsep f = hsep . map (\x -> ( if f x then parens else id ) $ pprint x)

instance PPrint UntypedName where
  pprint = pprint . name

instance PPrint Name where
  pprint (NUntyped n) = text n
--  pprint (NTyped n _) = text n
  pprint (NTyped n s) = text n <+> (brackets $ pprint s)

instance PPrint Pattern where
  pprint (PVar name)    = pprint name
  pprint (PCon name ps) = 
    let isSimple (PVar _)    = True
        isSimple (PCon _ []) = True
        isSimple _           = False
    in 
      pprint name <+> parensHsep (not . isSimple) ps

instance PPrint Match where
  pprint (Match pat e) = hsep [pprint pat, text "->", pprint e]

instance PPrint Binding where
  pprint (Binding n e) = hsep [ pprint n, text "=", pprint e ]

instance PPrint Expression where
  pprint (EVar name)   = pprint name
  pprint (ECon name)   = pprint name
  pprint (EApp f args) = parensHsep (not . isSimple) $ f : args
    where isSimple (EVar {})  = True
          isSimple (ECon {})  = True
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
        ++ (map (nest 2) $ punctuate (text " ;") $ map pprint matches)
        ++ [text "}"]

  pprint (ELet bs e) =
    vcat $ [ text "let {" ]
        ++ (map (nest 2) $ punctuate (text " ;") $ map pprint bs)
        ++ [ text "}", text "in", nest 2 $ pprint e ]

  pprint EUndefined = text "undefined"

instance PPrint Type where
  pprint (TVar name)   = pprint name
  pprint (TCon f args) = 
    case args of
      [a,b] | f == funName -> parensHsep (not . isSimple) [a,TVar f,b]
      _                    -> parensHsep (not . isSimple) $ (TVar f) : args
    where isSimple (TVar _)    = True
          isSimple (TCon _ []) = True
          isSimple _           = False

instance PPrint Scheme where
  pprint (SType t)     = pprint t
  pprint (SForall n s) = hsep [ text "forall", pprint n, text ".", pprint s ]

instance PPrint Constructor where
  pprint (CCon name types) = pprint $ TCon name types

instance PPrint Declaration where
  pprint (DBind binding) = pprint binding

  pprint (DAdt name tvars cons) =
    hsep [ text "adt", pprint name, hsep $ map pprint tvars, text "= {"] 
      $$ (nest 2 $ vcat $ punctuate (text " ;") $ map pprint cons)
      $$ (text "}")

instance PPrint Program where 
  pprint (Program main decs) = vcat $ punctuate (text ";") 
                                    $ map pprint 
                                    $ DBind main : decs

instance (PPrint a, PPrint b) => PPrint (a,b) where
  pprint (a,b) = parens $ hcat [pprint a, text ",", pprint b]
