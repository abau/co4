{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module CO4.Test.Eitherize
where

import           Prelude hiding (not,(&&),(||))
import qualified Language.Haskell.TH as TH
import           Satchmo.Boolean
import           Satchmo.Code (decode)
import           Satchmo.SAT.Mini (solve)
import           CO4
import           CO4.MonadifyTypes

$([d| data MyBool = Wahr | Falsch

      myNot b = case b of Wahr -> Falsch
                          Falsch -> Wahr

      myAnd a b = case a of 
        Wahr   -> 
          case b of 
            Wahr   -> Wahr
            Falsch -> Falsch
        Falsch -> Falsch

      foo a  b = myAnd (myNot a) b

   |] >>= \p -> TH.runIO $ compile p [ Verbose, NoRaml ]
  )

test :: IO ()
test = do
  mResult <- solve ( do a <- boolean
                        b <- boolean
                        let encA = EncEither a () ()
                            encB = EncEither b () ()
                        --notA <- myNot encA
                        EncEither r () () <- foo encB encA
                        assert [r]
                        return ( decode [a,b] )
                 ) :: IO (Maybe [Bool])
  putStrLn (unwords 
    [ "Test:"
    , show mResult
    ] )

