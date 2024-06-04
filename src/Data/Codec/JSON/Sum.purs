module Data.Codec.JSON.Sum where

import Prelude

import Codec.JSON.DecodeError as Error
import Control.Monad.Except (Except, except)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import JSON (JSON)
import JSON.Object as JO
import JSON.Path as JP

-- | A helper for defining JSON codecs for sum types. To ensure exhaustivity
-- | there needs to be a mapping to and from a tag type for the type to be
-- | encoded.
-- |
-- | - The first argument is the name of the type being decoded, for error
-- |   message purposes.
-- | - The second argument maps a tag value to a string to use in the encoding.
-- | - The third argument maps a string back to a tag value during decoding.
-- | - The fourth argument returns either a constant value or a decoder function
-- |   based on a tag value.
-- | - The fifth argument returns a tag value and optional encoded value to
-- |   store for a constructor of the sum.
taggedSum
  ∷ ∀ tag a
  . String
  → (tag → String)
  → (String → Maybe tag)
  → (tag → Either a (JSON → Either CJ.DecodeError a))
  → (a → Tuple tag (Maybe JSON))
  → CJ.Codec a
taggedSum name printTag parseTag f g = Codec.codec decodeCase encodeCase
  where
  decodeCase ∷ JSON → Except CJ.DecodeError a
  decodeCase j =
    except $ lmap (Error.withContext ("Could not decode " <> name)) do
      obj ← CJ.decode CJ.jobject j
      tag ← CJ.decode (CJ.prop "tag" CJ.string) obj
      case parseTag tag of
        Nothing →
          Left (Error.error (JP.AtKey "tag" JP.Tip) ("Unexpected value '" <> tag <> "' found"))
        Just t →
          case f t of
            Left a → pure a
            Right decoder → do
              value ← CJ.decode (CJ.prop "value" CJ.json) obj
              lmap (Error.withPath (JP.AtKey "value")) (decoder value)

  encodeCase ∷ a → JSON
  encodeCase a = case g a of
    Tuple tag value →
      Codec.encode CJ.jobject
        $ JO.fromEntries
        $ Array.catMaybes
            [ Just $ Tuple "tag" (Codec.encode CJ.string (printTag tag))
            , Tuple "value" <$> value
            ]

-- | A helper for defining JSON codecs for "enum" sum types, where every
-- | constructor is nullary, and the type will be encoded as a string.
enumSum ∷ ∀ a. (a → String) → (String → Maybe a) → CJ.Codec a
enumSum printTag parseTag = Codec.codec' decode encode
  where
  decode json = except do
    tag ← CJ.decode CJ.string json
    case parseTag tag of
      Nothing → Left (Error.basic $ "Unexpected value '" <> tag <> "' found")
      Just a → pure a

  encode = Codec.encode CJ.string <<< printTag
