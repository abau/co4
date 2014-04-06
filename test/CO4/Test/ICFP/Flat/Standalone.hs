module CO4.Test.ICFP.Flat.Standalone
where

import CO4.Prelude
import CO4.PreludeNat

type Memory = [E]
type Ptr    = Nat

data P = P Ptr deriving (Show)

data E = Null
       | One
       | Id
       | If  Ptr Ptr Ptr
       | Op1 Op1 Ptr
       | Op2 Op2 Ptr Ptr
       deriving (Show,Eq)

data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16 deriving (Show,Eq)

data Op2 = And | Or | Xor | Plus deriving (Show,Eq)

type Number = Nat
type Env    = Number
type Values = [Number]

constraint :: [(Number,Number)] -> (Memory,P) -> Bool
constraint samples u = all (\sample -> constraint' sample u) samples

nullNat        = nat 3 0
incrementNat n = plus'Nat (nat 3 1) n

constraint' :: (Number,Number) -> (Memory,P) -> Bool
constraint' (x,y) (memory,p) = 
  case p of 
    P ptr -> let env    = writeEnv x emptyEnv
                 values = foldl (\vs m -> vs ++ [ eval env vs m ]) [] memory
             in
               eqNat y (at ptr values)

shiftR4Nat  x = shiftRNat (shiftRNat (shiftRNat (shiftRNat x)))
shiftR16Nat x = shiftR4Nat (shiftR4Nat (shiftR4Nat (shiftR4Nat x)))

eval :: Env -> Values -> E -> Number
eval env vs expression = case expression of
  Null -> nat 64 0
  One  -> nat 64 1
  Id   -> readEnv env

  If c t f -> case isZeroNat (trimNat 3 (at c vs)) of
                True  -> at t vs
                False -> at f vs

  Op1 op1 e -> case op1 of
    Not   -> invertNat   (at e vs)
    Shl1  -> shiftLNat   (at e vs)
    Shr1  -> shiftRNat   (at e vs)
    Shr4  -> shiftR4Nat  (at e vs)
    Shr16 -> shiftR16Nat (at e vs)

  Op2 op2 e1 e2 -> case op2 of
    And  -> andNat   (at e1 vs) (at e2 vs)
    Or   -> orNat    (at e1 vs) (at e2 vs)
    Xor  -> xorNat   (at e1 vs) (at e2 vs)
    Plus -> plus'Nat (at e1 vs) (at e2 vs) 

at :: Number -> [a] -> a
at i xs = 
  let go j xs = case xs of
        []     -> undefined
        (y:ys) -> case eqNat i j of
          True  -> y
          False -> go (incrementNat j) ys
  in
    go nullNat xs

emptyEnv = nat 64 0

writeEnv :: Number -> Env -> Env
writeEnv n _ = n

readEnv :: Env -> Number
readEnv x = x
