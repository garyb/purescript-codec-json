module Test.Prim where

import Prelude

import Control.Monad.Gen as Gen
import Control.Monad.Gen.Common as GenC
import Data.Char.Gen (genAsciiChar)
import Data.Codec.JSON.Common ((~))
import Data.Codec.JSON.Common as CJ
import Data.Either (Either(..), either, note)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Data.String.Gen (genAsciiString)
import Effect (Effect)
import Effect.Console (log)
import JSON as J
import JSON.Gen (genJArray, genJObject)
import JSON.Object as JO
import Test.QuickCheck (Result(..), quickCheck)
import Test.QuickCheck.Gen (Gen)
import Test.Util (genInt, propCodec, propCodec', propCodec'')

main ∷ Effect Unit
main = do
  log "Checking JNull codec"
  quickCheck propNull

  log "Checking Boolean codec"
  quickCheck propBoolean

  log "Checking Number codec"
  quickCheck propNumber

  log "Checking Int codec"
  quickCheck propInt

  log "Checking String codec"
  quickCheck propString

  log "Checking Char codec"
  quickCheck propChar

  log "Checking JArray codec"
  quickCheck propJArray

  log "Checking JObject codec"
  quickCheck propJObject

  log "Checking object codec"
  quickCheck (propTestRecord codecObject)

  log "Checking record codec"
  quickCheck (propTestRecord codecRecord)

  log "Checking record codec with optional field"
  quickCheck propTestRecordOptional

  log "Checking record codec with optional field does include the field"
  quickCheck propPresentOptionalField

  log "Checking record codec with optional field does omit the field entirely"
  quickCheck propMissingOptionalField

  log "Checking fixed-point codec"
  quickCheck propFix

  log "Checking nullable codec"
  quickCheck propNullable

propNull ∷ Gen Result
propNull = propCodec (pure unit) CJ.null

propBoolean ∷ Gen Result
propBoolean = propCodec Gen.chooseBool CJ.boolean

propNumber ∷ Gen Result
propNumber = propCodec (Gen.chooseFloat (-100000.0) 100000.0) CJ.number

propInt ∷ Gen Result
propInt = propCodec genInt CJ.int

propString ∷ Gen Result
propString = propCodec genAsciiString CJ.string

propChar ∷ Gen Result
propChar = propCodec genAsciiChar CJ.char

propJArray ∷ Gen Result
propJArray = propCodec'' (J.print <<< J.fromJArray) genJArray CJ.jarray

propJObject ∷ Gen Result
propJObject = propCodec'' (J.print <<< J.fromJObject) genJObject CJ.jobject

propNullable ∷ Gen Result
propNullable = propCodec (GenC.genMaybe genInt) (CJ.nullable CJ.int)

type TestRecord = { tag ∷ String, x ∷ Int, y ∷ Boolean }

genRecord ∷ Gen TestRecord
genRecord =
  { tag: _, x: _, y: _ }
    <$> genAsciiString
    <*> genInt
    <*> Gen.chooseBool

codecObject ∷ CJ.Codec TestRecord
codecObject =
  CJ.object $
    { tag: _, x: _, y: _ }
      <$> _.tag ~ CJ.prop "tag" CJ.string
      <*> _.x ~ CJ.prop "x" CJ.int
      <*> _.y ~ CJ.prop "y" CJ.boolean

codecRecord ∷ CJ.Codec TestRecord
codecRecord =
  CJ.object $ CJ.record
    # CJ.recordProp @"tag" CJ.string
    # CJ.recordProp @"x" CJ.int
    # CJ.recordProp @"y" CJ.boolean

propTestRecord ∷ CJ.Codec TestRecord → Gen Result
propTestRecord = propCodec' checkEq print genRecord
  where
  checkEq r1 r2 = r1.tag == r2.tag && r1.x == r2.x && r1.y == r2.y
  print { tag, x, y } =
    "{ tag: " <> show tag <> ", x: " <> show x <> ", y: " <> show y <> " }"

type TestRecordOptional = { tag ∷ String, x ∷ Maybe Int }

genRecordOptional ∷ Gen TestRecordOptional
genRecordOptional =
  { tag: _, x: _ }
    <$> genAsciiString
    <*> GenC.genMaybe genInt

codecRecordOptional ∷ CJ.Codec TestRecordOptional
codecRecordOptional =
  CJ.object $ CJ.record
    # CJ.recordProp @"tag" CJ.string
    # CJ.recordPropOptional @"x" CJ.int

propTestRecordOptional ∷ Gen Result
propTestRecordOptional = propCodec' checkEq print genRecordOptional codecRecordOptional
  where
  checkEq r1 r2 = r1.tag == r2.tag && r1.x == r2.x
  print { tag, x } =
    case x of
      Just _ → "{ tag: " <> show tag <> ", x: " <> show x <> " }"
      Nothing → "{ tag: " <> show tag <> " }"

propPresentOptionalField ∷ Gen Result
propPresentOptionalField = do
  tag ← genAsciiString
  x ← genInt
  let value = { tag, x: Just x }
  let json = CJ.encode codecRecordOptional value
  pure $ either Failed (pure Success) do
    obj ← note "Encoded JSON is not an object" $ J.toJObject json
    prop ← note "Optional property unexpectedly missing in object" $ JO.lookup "x" obj
    n ← note "x value is not a plain number" $ J.toNumber prop
    if n == Int.toNumber x then pure unit
    else Left "x value is wrong"

propMissingOptionalField ∷ Gen Result
propMissingOptionalField = do
  tag ← genAsciiString
  let value = { tag, x: Nothing }
  let json = CJ.encode codecRecordOptional value
  pure $ either Failed (pure Success) do
    obj ← note "Encoded JSON is not an object" $ J.toJObject json
    maybe (Right Success) (\_ → Left "Optional property unexpectedly appeared in object") $ JO.lookup "x" obj

newtype FixTest = FixTest (Maybe FixTest)

derive instance Newtype FixTest _
derive instance Generic FixTest _
derive instance Eq FixTest

instance Show FixTest where
  show x = genericShow x

genFixTest ∷ Gen FixTest
genFixTest = Gen.sized \n →
  if n <= 1 then pure $ FixTest Nothing
  else FixTest <$> Gen.resize (_ - 1) (GenC.genMaybe genFixTest)

codecFixTest ∷ CJ.Codec FixTest
codecFixTest = CJ.fix \codec →
  dimap unwrap wrap (CJ.maybe codec)

propFix ∷ Gen Result
propFix = propCodec genFixTest codecFixTest
