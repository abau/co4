{-# LANGUAGE FlexibleInstances #-}
-- |String frontend
module CO4.Frontend.String
  (parseProgramFromFile)
where

import           Control.Applicative ((<$>))
import           Data.Char (isUpper,isLower)
import           Text.Parsec hiding (string,char)
import qualified Text.Parsec.Token as T
import           Text.Parsec.Language (haskellDef)
import           Text.Parsec.String (Parser)
import           CO4.Language
import           CO4.Frontend
import           CO4.Names (fromName,funType)

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
pProgram = many1 pDeclaration

pDeclaration :: Parser Declaration
pDeclaration = do
  Name n <- pVarName
  name   <- option (Name n) $ reservedOp "::" >> pScheme >>= return . TypedName n
  symbol "="
  e <- pExpression
  symbol ";"
  return $ DBind name e

pExpression :: Parser Expression
pExpression = (try pEApp) <|> pELam <|> pEVar <|> pECon <|> pECase <|> pELet 
              <|> pESignedLit <|> parens pExpression <?> "expression"

pEVar, pECon , pEApp :: Parser Expression
pEVar = EVar <$> pVarName
pECon = ECon <$> pConName

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
  let rMatch = do
        pat <- pPattern
        reservedOp "->"
        exp <- pExpression
        symbol ";"
        return $ Match pat exp
  in do
    reserved "case"
    e <- pExpression 
    reserved "of"
    ECase e <$> (braces $ many1 rMatch)

pELet = do
  reserved "let"
  name <- pVarName
  symbol "="
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
  p <- pConName
  return $ PCon p []

pPCon :: Parser Pattern
pPCon = 
  let simpleP = pPVar <|> pPUnsignedLit <|> pNonArgPCon <|> parens pPattern 
                <?> "variable pattern, unsigned literal pattern, non argument constructor pattern, pattern in parentheses"
  in do
    name <- pConName
    PCon name <$> many1 simpleP

pType :: Parser Type
pType = (try pFunctionTCon) <|> pNonFunctionalType <?> "type"

pNonFunctionalType :: Parser Type
pNonFunctionalType = pTCon <|> pTVar <|> parens pType <?> "non-functional type"

pTVar :: Parser Type
pTVar = TVar <$> pVarName

pNonArgTCon :: Parser Type
pNonArgTCon = flip TCon [] <$> pTypeName

pTCon :: Parser Type
pTCon = 
  let simpleT = pTVar <|> pNonArgTCon <|> parens pType 
                <?> "type variable, non argument type constructor or type in parenthesis"
  in 
    choice [ do c <- pTypeName
                TCon c <$> many simpleT
           , try $ do c <- pVarName
                      -- Type constructor variables must have >0 arguments
                      TCon c <$> many1 simpleT 
           ]

pFunctionTCon :: Parser Type
pFunctionTCon = do
  a <- pNonFunctionalType
  reservedOp $ fromName funType
  b <- pType
  return $ TCon funType [a,b]

pScheme :: Parser Scheme
pScheme = pSForall <|> pSType <|> parens pScheme <?> "scheme"

pSType :: Parser Scheme
pSType = SType <$> pType

pSForall :: Parser Scheme
pSForall = do
  TVar v <- reserved "forall" >> pTVar
  symbol "." 
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
pTypeName :: Parser Name
pTypeName = pConName

pVarName :: Parser Name
pVarName = try $ choice [ do i <- identifier 
                             if isLower (head i) 
                               then return (Name i) 
                               else parserZero
                        , Name <$> operator
                        ]

pConName :: Parser Name
pConName = try $ do i <- identifier 
                    if isUpper (head i) then return (Name i) else parserZero

lexer           = T.makeTokenParser haskellDef    
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
