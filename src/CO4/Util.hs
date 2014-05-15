-- |Utility functions
module CO4.Util
where

import           Control.Exception (assert)
import           Data.List (partition,find)
import           Data.Maybe (mapMaybe)
import           Data.Typeable (Typeable)
import           Data.Generics (extM,extT)
import           Text.Read (readEither)
import           CO4.Language
import           CO4.Names as N

-- |Gets all declarations of a program
programDeclarations :: Program -> [Declaration]
programDeclarations (Program main decls) = (DBind main) : decls

-- |Gets all top-level bindings of a program
programToplevelBindings :: Program -> [Binding]
programToplevelBindings (Program main decls) = main : (mapMaybe fromDecl decls)

  where fromDecl (DBind b) = Just b
        fromDecl _         = Nothing

-- |Gets the name of the main binding
mainName :: Program -> Name
mainName = boundName . pMain

-- |Builds a program from a list of declarations. Fails if main binding is not found.
programFromDeclarations :: [Declaration] -> Program
programFromDeclarations decls  = case partition isMain decls of
  ([DBind main],rest) -> Program main rest
  ([],_) -> error $ "Util.programFromDeclarations: no top-level binding '" ++ N.mainName ++ "' or '" ++ deprecatedMainName ++ "' found"
  (_,_)  -> error $ "Util.programFromDeclarations: multiple top-level bindings '" ++ N.mainName ++ "' or '" ++ deprecatedMainName ++ "' found"
  
  where isMain (DBind (Binding name _)) = name == N.mainName 
                                       || name == deprecatedMainName
        isMain _                        = False

-- |Finds a top-level binding by its name
toplevelBindingByName :: Name -> Program -> Maybe Binding
toplevelBindingByName name = find (\(Binding n _) -> n == name) . programToplevelBindings

-- |Splits top-level declarations into type declarations and value declarations
splitDeclarations :: Program -> ([Adt], [Binding], [Signature])
splitDeclarations = foldl split ([],[],[]) . programDeclarations
  where split (types, vals, sigs) (DAdt  a) = (types ++ [a], vals, sigs)
        split (types, vals, sigs) (DBind b) = (types, vals ++ [b], sigs)
        split (types, vals, sigs) (DSig  s) = (types, vals, sigs ++ [s])

-- |Adds declarations to a program
addDeclarations :: [Declaration] -> Program -> Program
addDeclarations decls program = program { pDecls = pDecls program ++ decls }

-- |Finds a signature by its name
findSignature :: UntypedName -> [Signature] -> Maybe Scheme
findSignature n = fmap (\(Signature _  s) -> s) 
                . find (\(Signature n' _) -> n == n')

-- |Removes a signature from a program
removeSignature :: UntypedName -> Program -> Program
removeSignature name (Program main decls) = Program main $ mapMaybe go decls
  where
    go (DSig (Signature name' _)) | name == name' = Nothing
    go decl                                       = Just decl

-- |Gets all constructor's argument types
allConstructorsArgumentTypes :: Adt -> [Type]
allConstructorsArgumentTypes = concatMap cConArgumentTypes . adtConstructors

-- |Replaces an element at a certain position in a list
replaceAt :: Integral i => i -> a -> [a] -> [a]
replaceAt _ _ []     = []
replaceAt 0 y (_:xs) = y : xs
replaceAt i y (x:xs) = x : ( replaceAt (i-1) y xs )

-- |Replaces the first element in a list that matches a predicate 
replaceBy :: (a -> Bool) -> a -> [a] -> [a]
replaceBy _ _ [] = []
replaceBy f y (x:xs) | f x       = y : xs
replaceBy f y (x:xs) | otherwise = x : ( replaceBy f y xs )

-- |Checks whether a list has length one
lengthOne :: [a] -> Bool
lengthOne l = case l of [_] -> True ; _ -> False

-- |Checks whether all elements of a list are equal
equals :: Eq a => [a] -> Bool
equals as = all (== head as) $ tail as

-- |@equal f = equals . map f@, so you can write @equal length xss@
equal :: Eq b => (a -> b) -> [a] -> Bool
equal f = equals . map f

-- |@for = flip map@
for :: [a] -> (a -> b) -> [b]
for = flip map

-- * Smart constructors using 'Namelike'
-- There are also redefinitions of constructors
-- without namelike parameters, for the sake of consistent code.

tVar :: Namelike n => n -> Type
tVar = TVar . untypedName

tCon :: Namelike n => n -> [Type] -> Type
tCon n = TCon $ untypedName n

sType :: Type -> Scheme
sType = SType

sForall :: Namelike n => n -> Scheme -> Scheme
sForall n = SForall $ untypedName n

nUntyped :: Namelike n => n -> Name
nUntyped = NUntyped . fromName

nTyped :: Namelike n => n -> Scheme -> Name
nTyped n = NTyped $ fromName n

pVar :: Namelike n => n -> Pattern
pVar = PVar . name

pCon :: Namelike n => n -> [Pattern] -> Pattern
pCon n = PCon $ name n

match :: Pattern -> Expression -> Match
match = Match

binding :: Namelike n => n -> Expression -> Binding
binding n = Binding $ name n

eVar :: Namelike n => n -> Expression
eVar = EVar . name

eCon :: Namelike n => n -> Expression
eCon = ECon . name

eApp :: Expression -> [Expression] -> Expression
eApp = EApp

eApp' :: Expression -> Expression -> Expression
eApp' a b = EApp a [b]

eApp'' :: Expression -> Expression -> Expression -> Expression
eApp'' a b c = EApp a [b,c]

eTApp :: Expression -> [Type] -> Expression
eTApp = ETApp

eLam :: Namelike n => [n] -> Expression -> Expression
eLam ns = ELam (map name ns)

eTLam :: Namelike n => [n] -> Expression -> Expression
eTLam ns = ETLam (map untypedName ns)

eCase :: Expression -> [Match] -> Expression
eCase = ECase

eLet :: [Binding] -> Expression -> Expression
eLet = ELet 

cCon :: Namelike n => n -> [Type] -> Constructor
cCon n = CCon (untypedName n)

dBind :: Binding -> Declaration
dBind = DBind 

adt :: (Namelike n, Namelike m) => n -> [m] -> [Constructor] -> Adt
adt n m = Adt (untypedName n) (map untypedName m)

-- |Pattern-to-expression transformation
patternToExpression :: Pattern -> Expression
patternToExpression pat = case pat of
  PVar n    -> EVar n
  PCon n [] -> ECon n
  PCon n ps -> EApp (ECon n) $ map patternToExpression ps

-- |Expression-to-pattern transformation
expressionToPattern :: Expression -> Pattern 
expressionToPattern exp = case exp of
  EVar n           -> PVar n
  ECon n           -> PCon n []
  EApp (ECon n) es -> PCon n $ map expressionToPattern es

-- |@toBinary n i@ converts @i@ to its binary representation using @n@ bits.
-- Least significant bit is result's head.
toBinary :: Integral i => Maybe Int -> i -> [Bool]
toBinary numBits i = case numBits of
  Nothing -> result
  Just n  -> assert (length result <= n) 
           $ result ++ replicate (n - length result) False 
  where 
    result = go i
    go 0   = [False]
    go 1   = [True]
    go i   = case i `quotRem` 2 of
               (i',0) -> False : (go i')
               (i',1) -> True  : (go i')

-- |@fromBinary i@ converts @i@ from its binary representation to an @Int@.
-- Least significant bit is expecteted as @i@'s head.
fromBinary :: Integral i => [Bool] -> i
fromBinary = go 0
  where
    go _ []         = error "Util.fromBinary: empty list"
    go i [True]     = 2^i 
    go _ [False]    = 0
    go i (True:xs)  = 2^i + go (i+1) xs
    go i (False:xs) =       go (i+1) xs

-- |@binaries n@ returns all binary numbers with bit width @n@
binaries :: Int -> [[Bool]]
binaries 0 = [[]]
binaries i = do
  y <- binaries $ i - 1
  x <- [False,True]
  return $ x : y

-- |@bitWidth n@ returns the number of bits needed to encode @n@ different states
bitWidth :: Integral i => i -> Int
bitWidth 0 = 0
bitWidth n = ceiling $ logBase 2 $ fromIntegral n

-- |Checks whether a string represents an integer
isInt :: String -> Bool
isInt s = case readEither s :: Either String Int of
  Left _  -> False
  Right _ -> True

-- |'mapAndUnzipM' on 3-tuple
mapAndUnzip3M :: (Monad m) => (a -> m (b,c,d)) -> [a] -> m ([b],[c],[d])
mapAndUnzip3M f xs =  sequence (map f xs) >>= return . unzip3

-- |@extM' = flip 'extM'@
extM' :: (Monad m, Typeable a, Typeable b) => (b -> m b) -> (a -> m a) -> a -> m a
extM' = flip extM 

-- |@extT' = flip 'extT'@
extT' :: (Typeable a, Typeable b) => (b -> b) -> (a -> a) -> a -> a
extT' = flip extT 
