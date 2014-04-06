{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}

import Prelude hiding (Left, Right)
import Autolib.ToDoc

data List a = Nil | Cons a (List a)

toList l = case l of
    Nil -> [] ; Cons x xs -> x : toList xs

instance ToDoc a => ToDoc (List a) where
    toDoc = toDoc . toList

data Sigma = A | B | C | D

type Word = List Sigma

data Rule = Rule { lhs :: Word, rhs :: Word }

data Step = Step { prefix :: List Sigma
                 , rule :: Rule
                 , suffix :: List Sigma
                 }

data OStep = OStep Rule Overlap 

data Side = Left | Right | Inside | Outside

data Overlap = Overlap { side :: Side, pre :: Word, suf :: Word, c1 :: Rule, c2 :: Rule }

data Move = Move { origin :: List Sigma
                 , image :: List (List Sigma)
                 , derivation :: List Step
                 }

-- type Morphism = (List Move)

data Transport = Transport { pivot :: List Sigma
                           , morphism :: List Move
                           , start :: List Sigma
                           , images :: List Image
                           }

data Image = Image { power :: List (List Sigma) -- ^  phi^k (start)
                   , seed :: List (List Sigma) -- ^  start ^ pivot^k
                   }


derives [makeToDoc] [''Sigma, ''Rule, ''Step, ''OStep, ''Side, ''Overlap, ''Move, ''Transport, ''Image]

instance Show (List OStep) where show = render . toDoc
instance Show Transport where show = render . toDoc
