-- | encoding for the "1:n" picture hanging problem
-- Section 3 of http://arxiv.org/abs/1203.3602

module CO4.Example.HangStandalone where

import CO4.Prelude
import Data.List (inits,tails)

type Pin = Nat
data Dir = L | R deriving (Show, Eq)
data Turn = Turn Dir Pin deriving (Show, Eq)
type Hang = [ Turn ]

constraint :: Nat -> (Hang, [Pin]) -> Bool
constraint s (h, ps) =
     forall h ( \ t -> case t of Turn d p -> leNat p s )
  &&  primitive h
  && forall ps ( \ p -> nullable p h )

forall xs p = all p xs

matching :: Turn -> Turn -> Bool
matching (Turn d1 p1) (Turn d2 p2) =
  not (eqDir d1 d2) && eqNat p1 p2

eqDir d1 d2 = case d1 of
  L -> case d2 of L -> True ; R -> False
  R -> case d2 of L -> False ; R -> True

primitive :: Hang -> Bool
primitive h = not ( or ( zipWith matching h ( tail h ) ))

-- * reducibility checking that relies on caching.

-- | does the Hang reduce to [] when pin p is removed?
-- with cache, this is equivalent to CYK parsing,
-- since the second argument is always a substring
-- of the full sequence.
nullable :: Pin -> Hang -> Bool
nullable p h = case h of
  [] -> True
  x:xs -> case xs of
    [] -> case x of Turn d q -> eqNat p q
    y:ys -> 
         ( matching x (last xs) && nullable p (init xs) )
      || or (map ( \(l,r) -> nullable p l && nullable p r)
               (nonempty_splits (x:xs)) )

nonempty_splits xs =  tail ( init ( splits xs ) )

splits xs = zip (inits xs) (tails xs)
