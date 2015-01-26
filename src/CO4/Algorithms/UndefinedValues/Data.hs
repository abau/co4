module CO4.Algorithms.UndefinedValues.Data
  ( CO4OptionalValue (..)
  , optionalValueTypeName, definedConstructorName, undefinedConstructorName, optionalValueType
  , encCO4OVUndefinedCons, encCO4OVDefinedCons
  , completelyDefined, withDefinedness
  , onUnwrapped1, onUnwrapped2, wrappedValues, wrappedValue, definedArgument
  )
where

import qualified Satchmo.Core.Primitive as P
import           CO4.Monad (CO4)
import           CO4.EncodedAdt 
import           CO4.Language
import           CO4.Names (Namelike, readName)

data CO4OptionalValue a = CO4OVUndefined
                        | CO4OVDefined a

optionalValueTypeName, definedConstructorName, undefinedConstructorName :: Namelike n => n
optionalValueTypeName    = readName "CO4OptionalValue"
definedConstructorName   = readName "CO4OVDefined"
undefinedConstructorName = readName "CO4OVUndefined"

optionalValueType :: Adt
optionalValueType = Adt optionalValueTypeName [UntypedName "a"]
                  [ CCon undefinedConstructorName []
                  , CCon definedConstructorName [TVar $ UntypedName "a"]
                  ]

encCO4OVUndefinedCons :: CO4 EncodedAdt
encCO4OVUndefinedCons = encodedConstructor 0 2 []

encCO4OVDefinedCons :: EncodedAdt -> CO4 EncodedAdt
encCO4OVDefinedCons a = encodedConstructor 1 2 [a]

completelyDefined :: EncodedAdt -> CO4 EncodedAdt
completelyDefined adt = 
  if isEmpty adt 
  then encCO4OVDefinedCons adt
  else do as'  <- mapM completelyDefined $ arguments' adt
          encCO4OVDefinedCons $ makeWithId (id' adt) (flags' adt) as' (isPrefixfree' adt)

withDefinedness :: Primitive -> EncodedAdt -> CO4 EncodedAdt
withDefinedness def adt = make [def] [adt] True

onUnwrapped1 :: (EncodedAdt -> CO4 EncodedAdt) -> EncodedAdt -> CO4 EncodedAdt
onUnwrapped1 f x = case wrappedValues [x] of
  Nothing -> encCO4OVUndefinedCons
  Just ([x'],definednesses) -> do
    def    <- P.and definednesses
    result <- f x' 
    withDefinedness def result

onUnwrapped2 :: (EncodedAdt -> EncodedAdt -> CO4 EncodedAdt) 
             ->  EncodedAdt -> EncodedAdt -> CO4 EncodedAdt
onUnwrapped2 f x y = case wrappedValues [x,y] of
  Nothing -> encCO4OVUndefinedCons
  Just ([x',y'],definednesses) -> do 
    def    <- P.and definednesses
    result <- f x' y'
    withDefinedness def result

wrappedValues :: [EncodedAdt] -> Maybe ([EncodedAdt], [Primitive])
wrappedValues = fmap (\(adts,flags) -> (adts, concat flags))
              . fmap unzip
              . sequence
              . map wrappedValue

wrappedValue :: EncodedAdt -> Maybe (EncodedAdt, [Primitive])
wrappedValue adt = 
  if isEmpty adt then error "Algorithms.UndefinedValues.Data.wrappedValue: missing wrapper on empty data"
  else case definedArgument adt of
    Nothing  -> Nothing
    Just (wrapped, definednessFlag) -> 
      case arguments wrapped of
        Nothing   -> Just (wrapped, [definednessFlag])
        Just args -> 
          case wrappedValues args of
            Nothing             -> Nothing
            Just (args', flags) -> Just $
              ( makeWithId (id' wrapped) (flags' wrapped) args' (isPrefixfree' wrapped)
              , definednessFlag : flags )

definedArgument :: EncodedAdt -> Maybe (EncodedAdt, Primitive)
definedArgument adt = case constantConstructorIndex 2 adt of
  Just 0 -> Nothing
  _      -> case (arguments adt, flags adt) of
    (Nothing,_) -> error "Algorithms.UndefinedValues.definedArgument: missing argument"
    (_,Nothing) -> error "Algorithms.UndefinedValues.definedArgument: missing definedness flag"
    (Just [x], Just [d]) -> Just (x, d)
    _ -> error "Algorithms.UndefinedValues.definedArgument: unexpected number of arguments or definedness flags"
