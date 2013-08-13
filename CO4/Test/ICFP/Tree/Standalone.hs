module CO4.Test.ICFP.Tree.Standalone
where

import CO4.PreludeNat

data P = P E deriving (Show)

data E = Null
       | One
       | Id  -- Id
--       | If E E E
--       | Fold E E E
       | Op1 Op1 E
       | Op2 Op2 E E
       deriving (Show)

data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16 deriving (Show)

data Op2 = And | Or | Xor | Plus deriving (Show)

--data Id = Id1 | Id2 | Id3 deriving (Show,Eq)

type Number = Nat

type Env = Number

constraint :: [(Number,Number)] -> P -> Bool
constraint samples p = all (\(x,y) -> eqNat y (eval p x)) samples

shiftR4Nat  x = shiftRNat (shiftRNat (shiftRNat (shiftRNat x)))
shiftR16Nat x = shiftR4Nat (shiftR4Nat (shiftR4Nat (shiftR4Nat x)))

eval :: P -> Number -> Number
eval (P e) number = 
  let go e env = case e of
        Null      -> nat 64 0
        One       -> nat 64 1
        Id        -> readEnv env
        Op1 op1 e -> case op1 of
          Not   -> invertNat   (go e env)
          Shl1  -> shiftLNat   (go e env)
          Shr1  -> shiftRNat   (go e env)
          Shr4  -> shiftR4Nat  (go e env)
          Shr16 -> shiftR16Nat (go e env)

        Op2 op2 e1 e2 -> case op2 of
          And  -> andNat   (go e1 env) (go e2 env)
          Or   -> orNat    (go e1 env) (go e2 env)
          Xor  -> xorNat   (go e1 env) (go e2 env)
          Plus -> plus'Nat (go e1 env) (go e2 env)
  in
    go e (writeEnv number emptyEnv)

emptyEnv = nat 64 0

writeEnv :: Number -> Env -> Env
writeEnv n _ = n

readEnv :: Env -> Number
readEnv env = env

{- 
eval :: P -> Number -> Number
eval (P e) number = 
  let go e env = case e of
        Null      -> nat 64 0
        One       -> nat 64 1
        Id i      -> readEnv i env
        If e1 t f -> case isZeroNat (go e1 env) of
                      False -> go t env
                      True  -> go f env
        Fold exp start f ->
          let nExp   = go exp env
              nStart = go start env
          in
            go f ( writeEnv Id2 (block7Nat nExp) (writeEnv' Id3 env (
              go f ( writeEnv Id2 (block6Nat nExp) (writeEnv' Id3 env (
                go f ( writeEnv Id2 (block5Nat nExp) (writeEnv' Id3 env (
                  go f ( writeEnv Id2 (block4Nat nExp) (writeEnv' Id3 env (
                    go f ( writeEnv Id2 (block3Nat nExp) (writeEnv' Id3 env (
                      go f ( writeEnv Id2 (block2Nat nExp) (writeEnv' Id3 env (
                        go f ( writeEnv Id2 (block1Nat nExp) (writeEnv' Id3 env (
                          go f ( writeEnv Id2 (block0Nat nExp) (writeEnv' Id3 env nStart))
                        )))
                      )))
                    )))
                  )))
                )))
              )))
            )))
        Op1 op1 e -> case op1 of
          Not   -> invertNat   (go e env)
          Shl1  -> shiftLNat   (go e env)
          Shr1  -> shiftRNat   (go e env)
          Shr4  -> shiftR4Nat  (go e env)
          Shr16 -> shiftR16Nat (go e env)

        Op2 op2 e1 e2 -> case op2 of
          And  -> andNat  (go e1 env) (go e2 env)
          Or   -> orNat   (go e1 env) (go e2 env)
          Xor  -> xorNat  (go e1 env) (go e2 env)
          Plus -> plusNat (go e1 env) (go e2 env)
  in
    go e (writeEnv Id1 number emptyEnv)

emptyEnv = (nat 64 0, nat 64 0, nat 64 0)

writeEnv :: Id -> Number -> Env -> Env
writeEnv id n (a,b,c) = case id of
  Id1 -> (n,b,c)
  Id2 -> (a,n,c)
  Id3 -> (a,b,n)

writeEnv' :: Id -> Env -> Number -> Env
writeEnv' id env n = writeEnv id n env

readEnv :: Id -> Env -> Number
readEnv id (a,b,c) = case id of
  Id1 -> a
  Id2 -> b
  Id3 -> c
  -}
