module Test.Main where

import Prelude

import Codec.JSON.DecodeError (DecodeError(..))
import Codec.JSON.DecodeError as Error
import Control.Alt ((<|>))
import Data.Codec.JSON ((~))
import Data.Codec.JSON as CJ
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import JSON as J
import JSON.Object as JO
import JSON.Path as JP
import Test.Common as Common
import Test.Generic as Generic
import Test.Prim as TestPrim
import Test.QuickCheck (quickCheck', (===))
import Test.Record as Record
import Test.Variant as Variant

main ∷ Effect Unit
main = do
  log "Checking instances"
  log "------------------------------------------------------------"
  testAlternative
  log "Checking Prim codecs"
  log "------------------------------------------------------------"
  TestPrim.main
  log ""
  log "Checking Common codecs"
  log "------------------------------------------------------------"
  Common.main
  log ""
  log "Checking Variant codecs"
  log "------------------------------------------------------------"
  Variant.main
  log ""
  log "Checking Record codecs"
  log "------------------------------------------------------------"
  Record.main
  log ""
  log "Checking Generic codecs"
  log "------------------------------------------------------------"
  Generic.main

testAlternative ∷ Effect Unit
testAlternative = do
  log "Test alternative error accumulation"
  let json = J.fromJObject $ JO.fromEntries [ Tuple "a" (J.fromJObject JO.empty) ]
  quickCheck' 1 do
    CJ.decode multiCodec json === Left
      ( DecodeError
          { path: JP.Tip
          , message: "Could not decode IntTuple"
          , causes:
              [ DecodeError
                  { path: JP.Tip
                  , message: "Failed to decode alternatives"
                  , causes:
                      [ Error.basic "Expected value of type Array"
                      , Error.noValueFound (JP.AtKey "fst" JP.Tip)
                      , Error.noValueFound (JP.AtKey "a" (JP.AtKey "value" JP.Tip))
                      , Error.noValueFound (JP.AtKey "a" (JP.AtKey "val" JP.Tip))
                      ]
                  }
              ]
          }
      )

codecT1 ∷ CJ.Codec (Tuple Int Int)
codecT1 =
  CJ.indexedArray $
    Tuple
      <$> fst ~ CJ.index 0 CJ.int
      <*> snd ~ CJ.index 1 CJ.int

codecT2 ∷ CJ.Codec (Tuple Int Int)
codecT2 =
  CJ.object $
    Tuple
      <$> fst ~ CJ.prop "fst" CJ.int
      <*> fst ~ CJ.prop "snd" CJ.int

codecT3 ∷ CJ.Codec (Tuple Int Int)
codecT3 =
  CJ.object $
    Tuple
      <$> fst ~ CJ.prop "a" codecIntValue
      <*> fst ~ CJ.prop "b" codecIntValue

codecIntValue ∷ CJ.Codec Int
codecIntValue =
  CJ.object (CJ.prop "value" CJ.int) <|> CJ.object (CJ.prop "val" CJ.int)

multiCodec ∷ CJ.Codec (Tuple Int Int)
multiCodec = CJ.named "IntTuple" (codecT1 <|> codecT2 <|> codecT3)
