module Test.Record where

import Prelude

import Control.Monad.Gen as Gen
import Control.Monad.Gen.Common as GenC
import Data.Codec.JSON.Common as CJ
import Data.Codec.JSON.Record as CJR
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (dimap)
import Data.String.Gen (genAsciiString)
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

derive instance newtypeOuter ∷ Newtype Outer _

instance showOuter ∷ Show Outer where
  show (Outer r) = "Outer " <> J.print (CJ.encode outerCodec r)

instance eqOuter ∷ Eq Outer where
  eq (Outer o1) (Outer o2) =
    o1.a == o2.a
      && o1.b == o2.b
      && case o1.c, o2.c of
        Nothing, Nothing → true
        Just i1, Just i2 → i1.n == i2.n && i1.m == i2.m
        _, _ → false

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

  pure unit
