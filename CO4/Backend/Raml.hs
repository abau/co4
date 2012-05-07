{-# LANGUAGE FlexibleInstances #-}
module CO4.Backend.Raml
  (displayPreprocessedRamlProgram)
where

import qualified Raml.RAMLTypes as R
import           Text.Parsec.Pos (initialPos)
import           CO4.Language
import           CO4.PPrint (pprint)
import           CO4.Backend
import           CO4.Names
import           CO4.Unique (Unique)
import           CO4.Backend.TH (isTupleCon)
import           CO4.Backend.RamlPreprocess (preprocess)
import           CO4.Algorithms.TypedNames (eraseTypedNames)

pos = initialPos "RamlBackend"

instance ExpressionBackend R.Exp where
  displayExpression exp = case eraseTypedNames exp of
    -- Constants
    ECon c | c == trueCon  -> R.ETrue pos
    ECon c | c == falseCon -> R.EFalse pos
    ELit (LInt i)          -> R.EToInt (R.ENum (fromIntegral i) pos) pos

    -- Variables
    EVar v                 -> R.EVar (fromName v) pos

    -- Lists
    ECon c | c == nilCon   -> R.ENil pos
    EApp (ECon c) [head',tail'] | c == consCon ->
      R.ECons (display head') (display tail') pos

    -- Tuples
    EApp (ECon c) es | isTupleCon c -> R.ETuple (map display es) pos

    -- Functions
    EApp (EVar (Name "not"))  [x]  -> R.EUnaryOp R.UNot    (display x)             pos
    EApp (EVar (Name "+"))  [x,y]  -> R.EBinOp R.Add       (display x) (display y) pos
    EApp (EVar (Name "-"))  [x,y]  -> R.EBinOp R.Sub       (display x) (display y) pos
    EApp (EVar (Name "*"))  [x,y]  -> R.EBinOp R.Mult      (display x) (display y) pos
    EApp (EVar (Name "/"))  [x,y]  -> R.EBinOp R.Div       (display x) (display y) pos
    EApp (EVar (Name "&&")) [x,y]  -> R.EBinOp R.And       (display x) (display y) pos
    EApp (EVar (Name "||")) [x,y]  -> R.EBinOp R.Or        (display x) (display y) pos
    EApp (EVar (Name "<"))  [x,y]  -> R.EBinOp R.Less      (display x) (display y) pos
    EApp (EVar (Name "<=")) [x,y]  -> R.EBinOp R.LessEq    (display x) (display y) pos
    EApp (EVar (Name "==")) [x,y]  -> R.EBinOp R.Eq        (display x) (display y) pos
    EApp (EVar (Name ">=")) [x,y]  -> R.EBinOp R.GreaterEq (display x) (display y) pos
    EApp (EVar (Name ">"))  [x,y]  -> R.EBinOp R.Greater   (display x) (display y) pos

    EApp (ECon c) [x]   -> display $ EApp (EVar c) [x]
    EApp (EVar v) [x]   -> R.EApp Nothing (fromName v)  (display x) pos

    EApp (ECon c) args  -> display $ EApp (EVar c) args
    EApp (EVar v) args  -> R.EApp Nothing (fromName v) (R.ETuple (map display args) pos) pos

    -- Matches
    ECase exp [ Match (PCon nil []) nilExp
              , Match (PCon cons [PVar x, PVar xs]) consExp
              ] | (nil == nilCon && cons == consCon) ->
                    R.EMatchL R.NonDest (display exp) (display nilExp)
                      (fromName x) (fromName xs) (display consExp) pos

    ECase exp [ Match (PLit (LInt 0)) nullExp
              , Match (PVar x) notNullExp
              ] -> R.EMatchN (display exp) (display nullExp) (fromName x) 
                      (display notNullExp) pos

    ECase exp [ Match (PCon true  []) trueExp
              , Match (PCon false []) falseExp
              ] | true == trueCon && false == falseCon ->
                  R.ECond (display exp) (display trueExp) (display falseExp) pos

    ECase _ [ Match (PCon tuple [PVar _, PVar _]) _ 
            ] | isTupleCon tuple -> tupleCase exp pos
    ECase _ [ Match (PCon tuple [PVar _, PVar _,PVar _]) _ 
            ] | isTupleCon tuple -> tupleCase exp pos
    ECase _ [ Match (PCon tuple [PVar _, PVar _,PVar _,PVar _]) _ 
            ] | isTupleCon tuple -> tupleCase exp pos
    ECase _ [ Match (PCon tuple [PVar _, PVar _,PVar _,PVar _,PVar _]) _ 
            ] | isTupleCon tuple -> tupleCase exp pos

    -- Local binding
    ELet name value exp -> R.ELet (fromName name) (display value) (display exp) pos

    _ -> error $ unlines ["Backend.Raml: don't know how to display expression",show $ pprint exp]

    where
      display = displayExpression

      tupleCase (ECase exp [ Match (PCon _ ps) tExp ]) pos =
        let patternVariables = map (\(PVar v) -> fromName v) ps
        in 
          R.EMatchT (display exp) patternVariables (display tExp) pos

instance SchemeBackend R.FunType where
  displayScheme scheme = case scheme of
    SForall _ _ -> error $ "Backend.Raml: can't display forall-quantified scheme '" ++ (show $ pprint scheme) ++ "'"
    SType (TCon c [a,b]) | c == funType ->
      case gatherFunType a b of
        ([p],result) -> R.FunType (displayType p) (displayType result)
        (ps ,result) -> R.FunType (R.TTuple $ map displayType ps) (displayType result)

    SType type_ -> R.FunType R.TUnit (displayType type_)

    where displayType type_ = case type_ of
            TCon c []  | c == intType  -> R.TInt
            TCon c []  | c == boolType -> R.TBool
            TCon c [t] | c == listType -> R.TList $ displayType t
            TCon c ts  | isTupleCon c  -> R.TTuple $ map displayType ts
            _ -> error $ "Backend.Raml: don't know how to display scheme '" ++ (show $ pprint scheme) ++ "'"

          gatherFunType a b =
            let gatherFunType' as (TCon c [a',b']) | c == funType = 
                  gatherFunType' (as ++ [a']) b'
                gatherFunType' as b'                              = (as,b')
            in
             gatherFunType' [a] b

instance ProgramBackend R.Program where
  displayProgram program = (concatMap displayDeclaration program, R.EUnit pos)

    where
      displayDeclaration (DBind (Name n) _) = error $ "Backend.Raml: can't display schemeless declaration '" ++ n ++ "'"
      displayDeclaration (DBind (TypedName n scheme) (ELam ns exp)) =
        [ R.TypeDec n (displayScheme scheme) pos
        , R.FunDec  n (map fromName ns) (displayExpression exp) pos
        ]
      displayDeclaration (DBind (TypedName n scheme) exp) =
        [ R.TypeDec n (displayScheme scheme) pos
        , R.FunDec  n [] (displayExpression exp) pos
        ]

displayPreprocessedRamlProgram :: Program -> Unique R.Program
displayPreprocessedRamlProgram p = preprocess p >>= return . displayProgram
