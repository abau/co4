{-# LANGUAGE FlexibleInstances #-}
-- |String frontend
module CO4.Frontend.String
  (parseProgramFromFile)
where

import           Control.Applicative ((<$>))
import           Data.Char (isUpper,isLower)
import           Text.Parsec hiding (string,char)
import qualified Text.Parsec.Token as T
import           Text.Parsec.Language (haskellStyle)
import           Text.Parsec.String (Parser)
import           CO4.Language
import           CO4.Frontend
import           CO4.Names (name,fromName,funType,untypedName,nTyped)

parseProgramFromFile :: FilePath -> IO Program
parseProgramFromFile filePath = unsafeParse pProgram False <$> readFile filePath

unsafeParse :: Parser a -> Bool -> String -> a
unsafeParse p showInError s = either (error . msg) id $ parse p' "" s
  where p' = do result <- whiteSpace >> p
                eof >> return result

        msg m = if showInError then concat [ "In\n\t",s,"\n", show m ]
                               else show m

instance ProgramFrontend String where
  parseProgram     s = return $ unsafeParse pProgram False s

instance ExpressionFrontend String where
  parseExpression  s = return $ unsafeParse pExpression True s

instance SchemeFrontend String where
  parseScheme      s = unsafeParse pScheme True s

pProgram :: Parser Program
pProgram = sepBy1 pDeclaration $ reservedOp ";"

pDeclaration :: Parser Declaration
pDeclaration = pAdt <|> pBinding <?> "declaration"

pAdt :: Parser Declaration
pAdt = do
  reserved "adt"
  n    <- pTypeName
  vars <- many pTypeVarName
  reservedOp "="
  DAdt n vars <$> (braces $ sepBy1 pConstructor $ reservedOp ";")

  where pConstructor = do
          TCon n ts <- pTConMono
          return $ CCon n ts

pBinding :: Parser Declaration
pBinding = do
  var  <- pVarName
  name <- option var $ reservedOp "::" >> pScheme >>= return . nTyped var
  reservedOp "="
  DBind name <$> pExpression

pExpression :: Parser Expression
pExpression = (try pEApp) <|> pELam <|> pEVar <|> pECon <|> pECase <|> pELet 
              <|> pESignedLit <|> parens pExpression <?> "expression"

pEVar, pECon , pEApp :: Parser Expression
pEVar = EVar <$> pVarName
pECon = ECon . name <$> pConName

pESignedLit :: Parser Expression
pESignedLit = ELit <$> pSignedLiteral

pEUnsignedLit :: Parser Expression
pEUnsignedLit = ELit <$> pUnsignedLiteral

pEApp = 
  let simpleE = pEVar <|> pECon <|> pEUnsignedLit <|> parens pExpression 
                <?> "variable, constructor, expression in parentheses"
  in do
    f    <- simpleE
    args <- many1 simpleE
    return $ EApp f args

pELam = do
  reservedOp "\\"
  names <- many1 pVarName
  reservedOp "->"
  e <- pExpression
  return $ ELam names e
  
pECase =
  let pMatch = do
        pat <- pPattern
        reservedOp "->"
        Match pat <$> pExpression
  in do
    reserved "case"
    e <- pExpression 
    reserved "of"
    ECase e <$> (braces $ sepBy1 pMatch $ reservedOp ";")

pELet = do
  reserved "let"
  name <- pVarName
  reservedOp "="
  value <- pExpression
  reserved "in"
  ELet name value <$> pExpression

pPattern :: Parser Pattern
pPattern = pPVar <|> pPSignedLit <|> try pPCon <|> pNonArgPCon <|> parens pPattern 
           <?> "pattern"

pPVar :: Parser Pattern
pPVar = PVar <$> pVarName

pPSignedLit :: Parser Pattern
pPSignedLit = PLit <$> pSignedLiteral

pPUnsignedLit :: Parser Pattern
pPUnsignedLit = PLit <$> pUnsignedLiteral

pNonArgPCon :: Parser Pattern
pNonArgPCon = do 
  p <- name <$> pConName
  return $ PCon p []

pPCon :: Parser Pattern
pPCon = 
  let simpleP = pPVar <|> pPUnsignedLit <|> pNonArgPCon <|> parens pPattern 
                <?> "variable pattern, unsigned literal pattern, non argument constructor pattern, pattern in parentheses"
  in do
    n <- name <$> pConName
    PCon n <$> many1 simpleP

pType :: Parser Type
pType = (try pFunctionTCon) <|> pNonFunctionalType <?> "type"

pNonFunctionalType :: Parser Type
pNonFunctionalType = pTCon <|> pTVar <|> parens pType <?> "non-functional type"

pTVar :: Parser Type
pTVar = TVar <$> pTypeVarName

pNonArgTCon :: Parser Type
pNonArgTCon = flip TCon [] <$> pTypeName

pSimpleType :: Parser Type
pSimpleType = pTVar <|> pNonArgTCon <|> parens pType 
  <?> "type variable, non argument type constructor or type in parenthesis"

pTCon :: Parser Type
pTCon = choice [ pTConMono, try pTConPoly ]

pTConMono :: Parser Type
pTConMono = do 
  c <- pTypeName
  TCon c <$> many pSimpleType

pTConPoly :: Parser Type
pTConPoly = do 
  c <- pTypeVarName
  -- Type constructor variables must have >0 arguments
  TCon c <$> many1 pSimpleType

pFunctionTCon :: Parser Type
pFunctionTCon = do
  a <- pNonFunctionalType
  reservedOp $ fromName funType
  b <- pType
  return $ TCon (untypedName funType) [a,b]

pScheme :: Parser Scheme
pScheme = pSForall <|> pSType <|> parens pScheme <?> "scheme"

pSType :: Parser Scheme
pSType = SType <$> pType

pSForall :: Parser Scheme
pSForall = do
  TVar v <- reserved "forall" >> pTVar
  reservedOp "." 
  SForall v <$> pScheme

pSignedLiteral :: Parser Literal
pSignedLiteral = signedNumberLiteral <|> LChar <$> char <?> "(signed) literal"

pUnsignedLiteral :: Parser Literal
pUnsignedLiteral = unsignedNumberLiteral <|> LChar <$> char <?> "unsigned literal"

unsignedNumberLiteral :: Parser Literal
unsignedNumberLiteral = do
  nOrD <- naturalOrDouble
  return $ case nOrD of
            Left  n -> LInt n
            Right d -> LDouble d

signedNumberLiteral :: Parser Literal
signedNumberLiteral = do
  sign <- option 1 $ choice [ symbol "+" >> return 1
                            , symbol "-" >> return (-1)]
  lit  <- unsignedNumberLiteral
  case lit of
    LInt i    -> return $ LInt    $ i * sign
    LDouble d -> return $ LDouble $ d * (fromIntegral sign)

-- Utilities
pTypeName :: Parser UntypedName
pTypeName = untypedName <$> pConName

pTypeVarName :: Parser UntypedName
pTypeVarName = untypedName <$> pVarName

pVarName :: Parser Name
pVarName = try $ choice [ do i <- identifier 
                             if isLower (head i) 
                               then return (NUntyped i) 
                               else parserZero
                        , NUntyped <$> operator
                        ]

pConName :: Parser UntypedName
pConName = try $ do i <- identifier 
                    if isUpper (head i) then return (UntypedName i) else parserZero

lexer           = T.makeTokenParser $ haskellStyle
                    { T.reservedOpNames= ["::", "=", "\\", "->", ";", "."]
                    , T.reservedNames  = ["let","in","case","of","if","then","else",
                                          "adt","forall"
                                         ]
                    }
parens          = T.parens lexer
braces          = T.braces lexer
identifier      = T.identifier lexer
reserved        = T.reserved lexer
whiteSpace      = T.whiteSpace lexer
reservedOp      = T.reservedOp lexer
operator        = T.operator lexer
symbol          = T.symbol lexer
naturalOrDouble = T.naturalOrFloat lexer
char            = T.charLiteral lexer
