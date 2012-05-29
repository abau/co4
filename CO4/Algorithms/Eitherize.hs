{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.Eitherize
  (eitherize)
where

import           Control.Applicative ((<$>))
import           Control.Monad.Identity
import           Control.Monad.Writer
import           Control.Monad.Reader
import qualified Data.Map as M
import           Data.List (find,findIndex)
import qualified Satchmo.Boolean as B
import           CO4.Language
import           CO4.Names (Namelike,untypedName,name,mapName,unitType,unitCon)
import           CO4.Algorithms.Instantiator
import           CO4.Frontend (parseProgram)
import           CO4.Frontend.String ()
import           CO4.PPrint (pprint)
import           CO4.Unique

import Debug.Trace

eitherAdt :: Unique [Declaration]
eitherAdt = 
  parseProgram $ unlines 
    [ "adt EncEither a b    = { EncEither Boolean a b } ;"
    , "booleanFromEncEither = \\e -> case e of { EncEither b x y -> b }"
    ]

newtype Instantiator a = Instantiator 
  { runInstantiator :: ReaderT [Declaration] Unique a } 
  deriving (Functor, Monad, MonadReader [Declaration], MonadUnique)

instance MonadInstantiator Instantiator where

  instantiateCon (ECon consName) = do
    Just adt <- findAdtByConstructorName consName <$> ask
    let Just consIndex    = findConstructorIndexByName consName adt
        callEncEitherCons = EApp $ ECon $ name "EncEither"

    case constructorArguments adt of
      [TCon left [], TCon right []] | left == unitType && right == unitType ->
        case consIndex of
          0 -> return $ callEncEitherCons [booleanTrue,unit,unit]
          1 -> return $ callEncEitherCons [booleanFalse,unit,unit]

  instantiateCase exp@(ECase e ms) = do
    e'  <- instantiate e
    [Match p1 m1, Match _ m2] <- instantiate ms
    Just adt@(DAdt _ [] cons) <- findAdtByPattern p1 <$> ask

    when (length cons /= length ms) $ error $ "No partial pattern match '" ++ (show $ pprint exp) ++ "' allowed"
    (pattern, flag) <- makeSatchmoConstructorPattern adt
    let callEncEitherCons = EApp $ ECon $ name "EncEither"
    return $ ECase e' 
      [Match pattern $ 
        ELet (name "left")  (EApp (EVar $ name "booleanFromEncEither") [m1]) $
        ELet (name "right") (EApp (EVar $ name "booleanFromEncEither") [m2]) $
        callEncEitherCons
          [ EApp (EVar $ name "&&")
              [ implies (EVar flag) (EVar $ name "left")
              , implies (EApp (EVar $ name "not") [EVar flag]) (EVar $ name "right")
              ]
          , unit
          , unit
          ]
      ]

implies :: Expression -> Expression -> Expression
implies a b = EApp (EVar $ name "||") [ EApp (EVar $ name "not") [a] , b ]

constructorArguments :: Declaration -> [Type]
constructorArguments = map toConsArg . dAdtConstructors
  where toConsArg (CCon _ [])  = TCon unitType []
        toConsArg (CCon _ [x]) = x
        toConsArg _            = error $ "Only one constructor argument allowed"

makeSatchmoConstructorPattern :: MonadUnique u => Declaration -> u (Pattern,Name)
makeSatchmoConstructorPattern adt =
  case constructorArguments adt of
    [TCon left [], TCon right []] | left == unitType && right == unitType -> do
      
      boolean <- newName "boolean"
      return ( PCon (name "EncEither") [PVar boolean, PCon unitCon [], PCon unitCon []]
             , boolean
             )

booleanTrue, booleanFalse, unit :: Expression
booleanTrue  = EApp (ECon $ name "Satchmo.Boolean.Constant") [ECon $ name "True"]
booleanFalse = EApp (ECon $ name "Satchmo.Boolean.Constant") [ECon $ name "False"]
unit         = ECon unitCon

findAdtByMatch :: Match -> [Declaration] -> Maybe Declaration
findAdtByMatch (Match pat _) = findAdtByPattern pat 

findAdtByPattern :: Pattern -> [Declaration] -> Maybe Declaration
findAdtByPattern pat = case pat of
  PCon pConName _ -> findAdtByConstructorName pConName
  _               -> const Nothing

findAdtByConstructorName :: Name -> [Declaration] -> Maybe Declaration
findAdtByConstructorName consName  = find byName
  where byName (DAdt _ _ cons) = any (eqConstructor consName) cons 

findConstructorByName :: Name -> Declaration -> Maybe Constructor
findConstructorByName n = find (eqConstructor n) . dAdtConstructors

findConstructorIndexByName :: Name -> Declaration -> Maybe Int
findConstructorIndexByName n = findIndex (eqConstructor n) . dAdtConstructors

eqConstructor :: Name -> Constructor -> Bool
eqConstructor n (CCon consName _) = n == name consName 
    
satchmoConstructorName :: Namelike n => n -> Name   
satchmoConstructorName = name . mapName ("Encoded" ++)

eitherize :: Program -> Unique Program
eitherize program = 
  let isAdt (DAdt {}) = True
      isAdt _         = False
      adts            = filter isAdt program 
  in do
    pEither  <- eitherAdt 
    program' <- runReaderT (runInstantiator $ instantiate program) adts
    return $ pEither ++ program'
