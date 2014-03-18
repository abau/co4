# CO4 - complexity concerned constraint compiler

CO4 enables a subset of Haskell to be used as a constraint specification language
through transformation into a satisfiability problem in propositional logic.

## Prerequisites

- [Haskell plattform] (http://www.haskell.org/platform/) with `ghc >= 7.6.1`
- [Minisat] (https://github.com/niklasso/minisat.git)
- [Minisat C bindings] (https://github.com/niklasso/minisat-c-bindings)
- [Minisat Haskell bindings] (https://github.com/niklasso/minisat-haskell-bindings)
- [Satchmo-core] (https://github.com/apunktbau/satchmo-core)

All other dependencies are available on Hackage and will be installed by 
cabal during the next step.

## Building

    cabal install

## Running

[CO4/Example/BinaryStandalone.hs] (https://github.com/apunktbau/co4/tree/master/CO4/Example/BinaryStandalone.hs)
implements addition and multiplication of binary values:

    fullAdder :: Bit -> Bit -> Bit -> (Bit,Bit)
    fullAdder a b carry =
      let xorAB = xor a b
      in
        ( xor xorAB carry
        , xor (a && b) (carry && xorAB)
        )

    halfAdder :: Bit -> Bit -> (Bit,Bit)
    halfAdder a b = (xor a b, a && b)

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
    ...

The toplevel constraint `constraint r (a,b)` is satisfied if 

 - `a * b = r` (without an overflow) and
 - `a > 1` and
 - `b > 1`

So:

    constraint r (a,b) = case mult a b of
      (result,carry) -> and [ result == r
                            , not (trivial a)
                            , not (trivial b)
                            , not carry
                            ]

[CO4/Example/Binary.hs] (https://github.com/apunktbau/co4/tree/master/CO4/Example/Binary.hs)
loads and compiles 
[CO4/Example/BinaryStandalone.hs] (https://github.com/apunktbau/co4/tree/master/CO4/Example/BinaryStandalone.hs)
using Template-Haskell:

    $( compileFile [ImportPrelude] "CO4/Example/BinaryStandalone.hs" )

Every definition `d` of the original program is compiled to an encoded
definition `encD`, i.e. the compiled constraint system is `encConstraint`.

As we're using definitions of Haskell's prelude (`map`,`foldl`,etc.), 
we pass the compiler flag `ImportPrelude`.

Next, we need to setup an allocator for the unknown values.
Allocators for types of the prelude (`uList`,`uBool`) are provided by CO4.

    bitWidth  = 8
    uNat      = uList bitWidth uBool

As `constraint` is a constraint over a pair of naturals, the final allocator is

    allocator = uTuple2 uNat uNat

Finally, we want to solve the compiled constraint system `encConstraint`.
CO4 provides several solving functions, e.g.
`solveAndTestP :: k -> Allocator -> ParamConstraintSystem -> (k -> a -> b) -> IO (Maybe a)`
solves a parametrized constraint system with 

 - `k` being the parameter
 - `Allocator` being an allocator for values of type `a`
 - `ParamConstraintSystem` being the parametrized constraint system
 - `(k -> a -> b)` being the original constraint system. The found solution is
 checked against this function in order to verify the solution.

In the main `constraint` we seek a factorization for a given natural number `x` of
bit-width `bitWidth` using the previously defined allocator:

    solution <- solveAndTestP (toBinary (Just bitWidth) x) allocator encMain main 

If there is a factorization, we want to decode the factors to decimal numbers:

    result :: Int -> IO (Maybe (Int,Int))
    result x = do
      solution <- solveAndTestP (toBinary (Just bitWidth) x) allocator encMain main 
      case solution of
        Nothing    -> return Nothing
        Just (a,b) -> return $ Just (fromBinary a, fromBinary b)
  
We call `result` in a ghci session to find a factorization of 143:

    $ ghci CO4/Example/Binary.hs
    *CO4.Example.Binary> result 143
    Start producing CNF
    Cache hits: 164 (10%), misses: 1457 (89%)
    CNF finished
    #variables: 7880, #clauses: 24448, #literals: 66292
    #clauses of length 1:	1
    #clauses of length 2:	7066
    #clauses of length 3:	17380
    #clauses of length 9:	2

    Starting solver
    Solver finished in 2.6667e-2 seconds (result: True)
    Starting decoder
    Decoder finished
    Test: True
    Just (13,11)

For more examples see [CO4/Example] (https://github.com/apunktbau/co4/tree/master/CO4/Example).

## Applications

- *RNA Design by Program Inversion via SAT solving* at [WCB 2013] (http://cp2013.a4cp.org/workshops/wcb) ([Proceedings] (http://cp2013.a4cp.org/sites/default/files/uploads/WCB13_proceedings.pdf))
- *Propositional Encoding of Constraints over Tree-Shaped Data* at [WFLP 2013] (http://www-ps.informatik.uni-kiel.de/wflp2013/) 
- *SAT compilation for Termination Proofs via Semantic Labelling* at [WST 2013] (http://www.imn.htwk-leipzig.de/WST2013/) ([Proceedings] (http://www.imn.htwk-leipzig.de/WST2013/papers/wst13.pdf))
