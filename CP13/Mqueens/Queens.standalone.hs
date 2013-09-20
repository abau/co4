import CO4.PreludeNat

constraint :: (Nat, [(Nat,Nat)]) -> [(Nat,Nat)] -> Bool
constraint (n, cells) queens = 
        onboard n queens
     && monotone ( map fst queens )
     && nonattack queens 
     && dominated cells queens

monotone xs = case xs of
    [] -> True
    x : ys -> case ys of
        [] -> True
        y : zs -> ltNat x y && monotone ys

onboard n queens = all ( \  q -> case q of
    (x,y) ->  leNat (nat 8 1) x && leNat x n
           && leNat (nat 8 1) y && leNat y n ) queens

attack (a,b) (c,d) = 
     eqNat a c || eqNat b d
  || eqNat (plusNat b c) (plusNat a d)
  || eqNat (plusNat a b) (plusNat c d)

nonattack qs = case qs of
    [] -> True
    q : qs' -> all ( \ q' -> not (attack q q')) qs'
            && nonattack qs'

dominated cells queens = case cells of
    [] -> True
    c : cells' -> any ( \ q -> attack c q ) queens
           && dominated cells' queens
