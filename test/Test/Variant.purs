module Test.Variant where

import Prelude

import Control.Monad.Gen (chooseBool, chooseInt)
import Control.Monad.Gen.Common as GenC
import Data.Codec.JSON.Common as CJ
import Data.Codec.JSON.Variant as CJV
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.String.Gen (genAsciiString)
import Data.Variant as V
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Gen (Gen)
import Test.Util (genInt, propCodec)
import Type.Proxy (Proxy(..))

type TestVariant = V.Variant
  ( a ∷ Int
  , b ∷ String
  , c ∷ Maybe Boolean
  )

main ∷ Effect Unit
main = do
  log "Checking Maybe-variant codec"
  quickCheck $
    propCodec
      (GenC.genMaybe genAsciiString)
      (codecMaybe CJ.string)
  log "Checking Maybe-variantMatch codec"
  quickCheck $
    propCodec
      (GenC.genMaybe genAsciiString)
      (codecMaybeMatch CJ.string)

  log "Checking Either-variant codec"
  quickCheck $
    propCodec
      (GenC.genEither genAsciiString genInt)
      (codecEither CJ.string CJ.int)

  log "Checking variant codec"
  quickCheck $
    propCodec genVariant codecVariant

codecMaybe ∷ ∀ a. CJ.Codec a → CJ.Codec (Maybe a)
codecMaybe codecA =
  dimap toVariant fromVariant
    ( CJV.variant
        # CJV.variantCase _Just (Right codecA)
        # CJV.variantCase _Nothing (Left unit)
    )
  where
  toVariant = case _ of
    Just a → V.inj _Just a
    Nothing → V.inj _Nothing unit
  fromVariant = V.case_
    # V.on _Just Just
    # V.on _Nothing (const Nothing)
  _Just = Proxy @"just"
  _Nothing = Proxy @"nothing"

codecMaybeMatch ∷ ∀ a. CJ.Codec a → CJ.Codec (Maybe a)
codecMaybeMatch codecA =
  dimap toVariant fromVariant
    ( CJV.variantMatch
        { just: Right codecA
        , nothing: Left unit
        }
    )
  where
  toVariant = case _ of
    Just a → V.inj (Proxy @"just") a
    Nothing → V.inj (Proxy @"nothing") unit
  fromVariant = V.match
    { just: Just
    , nothing: \_ → Nothing
    }

codecEither ∷ ∀ a b. CJ.Codec a → CJ.Codec b → CJ.Codec (Either a b)
codecEither codecA codecB =
  dimap toVariant fromVariant
    ( CJV.variant
        # CJV.variantCase _Left (Right codecA)
        # CJV.variantCase _Right (Right codecB)
    )
  where
  toVariant = case _ of
    Left a → V.inj _Left a
    Right b → V.inj _Right b
  fromVariant = V.case_
    # V.on _Left Left
    # V.on _Right Right
  _Left = Proxy @"left"
  _Right = Proxy @"right"

genVariant ∷ Gen TestVariant
genVariant = do
  tag ← chooseInt 1 3
  case tag of
    1 → V.inj (Proxy @"a") <$> genInt
    2 → V.inj (Proxy @"b") <$> genAsciiString
    _ → V.inj (Proxy @"c") <$> GenC.genMaybe chooseBool

codecVariant ∷ CJ.Codec TestVariant
codecVariant = CJV.variantMatch
  { a: Right CJ.int
  , b: Right CJ.string
  , c: Right (CJ.maybe CJ.boolean)
  }
