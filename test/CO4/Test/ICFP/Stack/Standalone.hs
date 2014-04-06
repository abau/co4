-- | idea: the program (tree) is represented 
-- as a list of statements for a stack machine.

module CO4.Test.ICFP.Stack.Standalone where

import CO4.Prelude
import CO4.PreludeNat

data Program = Program [Statement] deriving (Show)

data Statement 
       = Op0 Op0
       | Op1 Op1 
       | Op2 Op2 
       | Op3 Op3
       deriving (Show)

-- | these push one value (increase stack length)
data Op0 = Null | One | Id
     deriving ( Show )

-- | these pop one, and push one value (keep stack length)
data Op1 = Nop -- ^ since we fix the total program length
              -- we may need no-ops as fillers 
         | Not | Shl1 | Shr1 | Shr4 | Shr16 
     deriving (Show)

-- | these pop two values, and push one value (decrease stack length)
data Op2 = And | Or | Xor | Plus deriving (Show)

data Op3 = If0 deriving (Show)


type Number = Nat

type Env = Number

constraint :: [(Number,Number)] -> Program -> Bool
constraint samples p = all (\(x,y) -> eqNat y (eval p x)) samples

-- test case (for Standalone evaluation)
-- eval (Program [ Op0 One, Op0 Id, Op2 Plus  ]) (nat 64 3)  ==> (nat 64 4)

eval :: Program -> Number -> Number
eval (Program bs) x = eval_stack x bs []

type Stack = [ Number ]

shiftR4Nat  x = shiftRNat (shiftRNat (shiftRNat (shiftRNat x)))
shiftR16Nat x = shiftR4Nat (shiftR4Nat (shiftR4Nat (shiftR4Nat x)))

eval_stack :: Number-> [Statement] -> Stack -> Number
eval_stack env bs stack = case assertKnown bs of
    [] -> case stack of -- must have length 1 
        [] -> undefined
        y : ys -> case ys of
            [] -> y
            z : zs -> undefined
    b : bs' -> 
        let stack' = case b of
              Op0 op0 -> stack
              Op1 op1 -> tail stack
              Op2 op2 -> tail (tail stack)
              Op3 op3 -> tail (tail (tail stack))
            res = case b of
              Op0 op0 -> case op0 of
                          Null -> nat 64 0 
                          One  -> nat 64 1 
                          Id   -> env
              Op1 op1 -> let arg = head stack
                         in  case op1 of
                           Nop -> arg
                           Not -> invertNat arg
                           Shl1  -> shiftLNat   arg
                           Shr1  -> shiftRNat   arg
                           Shr4  -> shiftR4Nat  arg
                           Shr16 -> shiftR16Nat arg
              Op2 op2 -> let arg2 = head stack
                             arg1 = head (tail stack)
                         in  case op2 of
                              And  -> andNat   arg1 arg2
                              Or   -> orNat    arg1 arg2
                              Xor  -> xorNat   arg1 arg2
                              Plus -> plus'Nat arg1 arg2
              Op3 op3 -> let arg3 = head stack
                             arg2 = head (tail stack)
                             arg1 = head (tail (tail stack))
                         in  case op3 of
                              If0 -> case isZeroNat arg1 of
                                  True  -> arg2
                                  False -> arg3
        in  eval_stack env bs' ( res : stack' )
