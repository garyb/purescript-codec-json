module Test.Record where

import Prelude

import Codec.JSON.DecodeError as Error
import Control.Monad.Gen as Gen
import Control.Monad.Gen.Common as GenC
import Data.Bifunctor (lmap)
import Data.Codec.JSON.Common (Codec, boolean, decode, encode, int, maybe, object, string) as CJ
import Data.Codec.JSON.Record as CJR
import Data.Codec.JSON.Strict as CJS
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (dimap)
import Data.String.Gen (genAsciiString)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import JSON as J
import JSON.Object as JO
import Test.QuickCheck (assertEquals, quickCheck, quickCheckGen)
import Test.QuickCheck.Gen (Gen)
import Test.Util (genInt, propCodec)

type OuterR =
  { a ∷ Int
  , b ∷ String
  , c ∷ Maybe InnerR
  }

type InnerR =
  { n ∷ Int
  , m ∷ Boolean
  , o ∷ Maybe Boolean
  }

newtype Outer = Outer OuterR

derive instance Newtype Outer _
derive instance Eq Outer

instance Show Outer where
  show (Outer r) = "Outer " <> J.print (CJ.encode outerCodec r)

outerCodec ∷ CJ.Codec OuterR
outerCodec =
  CJ.object $ CJR.record
    { a: CJ.int
    , b: CJ.string
    , c: CJ.maybe innerCodec
    }

innerCodec ∷ CJ.Codec InnerR
innerCodec =
  CJ.object $ CJR.record
    { n: CJ.int
    , m: CJ.boolean
    , o: CJR.optional CJ.boolean
    }

innerCodecStrict ∷ CJ.Codec InnerR
innerCodecStrict =
  CJS.objectStrict $ CJR.recordStrict
    { n: CJ.int
    , m: CJ.boolean
    , o: CJR.optional CJ.boolean
    }

genOuter ∷ Gen OuterR
genOuter = do
  a ← genInt
  b ← genAsciiString
  c ← GenC.genMaybe genInner
  pure { a, b, c }

genInner ∷ Gen InnerR
genInner = do
  n ← genInt
  m ← Gen.chooseBool
  o ← GenC.genMaybe Gen.chooseBool
  pure { n, m, o }

main ∷ Effect Unit
main = do
  log "Checking record codec"
  quickCheck $ propCodec (Outer <$> genOuter) (dimap unwrap wrap outerCodec)

  log "Check optional Nothing is missing from json"
  quickCheckGen do
    v ← genInner
    let obj = J.toJObject $ CJ.encode innerCodec (v { o = Nothing })
    pure $ assertEquals (Just [ "m", "n" ]) (JO.keys <$> obj)

  log "Check optional Just is present in the json"
  quickCheckGen do
    b ← Gen.chooseBool
    v ← genInner
    let obj = J.toJObject $ CJ.encode innerCodec (v { o = Just b })
    pure $ assertEquals (Just [ "m", "n", "o" ]) (JO.keys <$> obj)

  log "Check ignoring unrecognized fields"
  quickCheckGen do
    b ← Gen.chooseBool
    n ← genInt
    let
      obj = J.fromJObject $ JO.fromEntries
        [ "m" /\ J.fromBoolean b
        , "n" /\ J.fromInt n
        , "bogus" /\ J.fromInt 42
        ]
    pure $ assertEquals (CJ.decode innerCodec obj) (Right { m: b, n, o: Nothing })

  log "Check failing on unrecognized fields"
  quickCheckGen do
    b ← Gen.chooseBool
    n ← genInt
    let
      obj = J.fromJObject $ JO.fromEntries
        [ "m" /\ J.fromBoolean b
        , "n" /\ J.fromInt n
        , "bogus" /\ J.fromInt 42
        ]
    pure $ assertEquals
      (lmap Error.print $ CJ.decode innerCodecStrict obj)
      (Left "Unknown field(s): bogus")

  pure unit
