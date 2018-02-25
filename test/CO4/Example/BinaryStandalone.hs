module CO4.Example.BinaryStandalone where

type Bit = Bool
type Nat = [Bit]

constraint r (a,b) = case mult a b of
  (result,carry) -> and [ eqNat' result r
                        , not (trivial a)
                        , not (trivial b)
                        , not carry
                        ]
trivial a = case a of
  []   -> True
  x:xs -> all not xs

mult :: Nat -> Nat -> (Nat,Bit)
mult a b = case a of
  []     -> ([],False)
  (x:xs) -> 
    let multResult  = mult xs b
        shiftResult = withCarry shiftRight multResult
    in
      case x of
        False -> shiftResult
        True  -> withCarry (add b) shiftResult

withCarry :: (Nat -> (Nat,Bit)) -> (Nat,Bit) -> (Nat,Bit)
withCarry f (n,carry) = case f n of
  (result, carry') -> (result, carry || carry')

shiftRight :: Nat -> (Nat,Bit)
shiftRight xs = cutMsb (False : xs)

cutMsb xs = case xs of
      []   -> ([],False) -- never happens
      y:ys -> case ys of
                [] -> ([],y)
                _  -> case cutMsb ys of 
                        (result,carry) -> (y : result,carry) 

add :: Nat -> Nat -> (Nat,Bit)
add a b = foldl (\(result,carry) (a,b) -> 
                    case fullAdder a b carry of
                      (r, carry') -> (result ++ [r], carry')
                ) ([],False) 
                  (case fill a b of
                      (a',b') -> zip a' b'
                  )

fill :: Nat -> Nat -> (Nat,Nat)
fill xs ys = case xs of
  []     -> case ys of []   -> ([],[])
                       v:vs -> case fill [] vs of
                                 (us',vs') -> (False : us', v : vs')
  u:us -> case ys of [] -> case fill us [] of
                             (us',vs') -> (u : us', False : vs')
                     v:vs -> case fill us vs of
                               (us',vs') -> (u:us', v:vs')

fullAdder :: Bit -> Bit -> Bit -> (Bit,Bit)
fullAdder a b carry =
  let xorAB = xor a b
  in
    ( xor xorAB carry
    , xor (a && b) (carry && xorAB)
    )

halfAdder :: Bit -> Bit -> (Bit,Bit)
halfAdder a b = (xor a b, a && b)
  
eqNat' :: Nat -> Nat -> Bool
eqNat' n1 n2 = case n1 of
  [] -> case n2 of [] -> True
                   _  -> False
  x:xs -> case n2 of []    -> False
                     y:ys  -> (not (xor x y)) && (eqNat' xs ys)

xor :: Bit -> Bit -> Bit
xor a b = case a of
  False -> b
  True  -> not b
