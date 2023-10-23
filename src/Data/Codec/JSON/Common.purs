module Data.Codec.JSON.Common
  ( nonEmptyString
  , nonEmptyArray
  , maybe
  , tuple
  , either
  , list
  , nonEmptyList
  , map
  , strMap
  , set
  , nonEmptySet
  , foreignObject
  , module Exports
  ) where

import Prelude hiding (identity, map, void)

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Codec as Codec
import Data.Codec.JSON (Codec, DecodeError(..), IndexedCodec, PropCodec, array, boolean, char, codePoint, coercible, decode, encode, fix, hoist, identity, index, indexedArray, int, jarray, jobject, json, named, null, nullable, number, object, prismaticCodec, prop, record, recordProp, recordPropOptional, string, void, (<~<), (>~>), (~)) as Exports
import Data.Codec.JSON ((~))
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Sum (taggedSum)
import Data.Codec.JSON.Sum (taggedSum) as Exports
import Data.Either (Either(..))
import Data.Functor as F
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Set as Set
import Data.Set.NonEmpty as NESet
import Data.String.NonEmpty as NEString
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Foreign.Object as Object
import JSON.Object as JO

-- | A codec for `NonEmptyString` values.
-- |
-- | Encodes as the standard type in JSON, but will fail to decode if the string is empty.
nonEmptyString ∷ CJ.Codec NEString.NonEmptyString
nonEmptyString = CJ.prismaticCodec "NonEmptyString" NEString.fromString NEString.toString CJ.string

-- | A codec for `NonEmptyArray` values.
-- |
-- | Encodes as the standard type in JSON, but will fail to decode if the array is empty.
nonEmptyArray ∷ ∀ a. CJ.Codec a → CJ.Codec (NEA.NonEmptyArray a)
nonEmptyArray codec = CJ.prismaticCodec "NonEmptyArray" NEA.fromArray NEA.toArray (CJ.array codec)

-- | A codec for `Maybe` values.
maybe ∷ ∀ a. CJ.Codec a → CJ.Codec (Maybe a)
maybe codec = taggedSum "Maybe" printTag parseTag dec enc
  where
  printTag = case _ of
    false → "Nothing"
    true → "Just"
  parseTag = case _ of
    "Nothing" → Just false
    "Just" → Just true
    _ → Nothing
  dec = case _ of
    false → Left Nothing
    true → Right (F.map Just <<< CJ.decode codec)
  enc = case _ of
    Nothing → Tuple false Nothing
    Just a → Tuple true (Just (CJ.encode codec a))

-- | A codec for `Tuple` values.
-- |
-- | Encodes as a two-element array in JSON.
tuple ∷ ∀ a b. CJ.Codec a → CJ.Codec b → CJ.Codec (Tuple a b)
tuple codecA codecB = CJ.named "Tuple" $ CJ.indexedArray $
  Tuple
    <$> fst ~ CJ.index 0 codecA
    <*> snd ~ CJ.index 1 codecB

-- | A codec for `Either` values.
either ∷ ∀ a b. CJ.Codec a → CJ.Codec b → CJ.Codec (Either a b)
either codecA codecB = taggedSum "Either" printTag parseTag dec enc
  where
  printTag = case _ of
    true → "Left"
    false → "Right"
  parseTag = case _ of
    "Left" → Just true
    "Right" → Just false
    _ → Nothing
  dec = case _ of
    true → Right (F.map Left <<< CJ.decode codecA)
    false → Right (F.map Right <<< CJ.decode codecB)
  enc = case _ of
    Left a → Tuple true (Just (CJ.encode codecA a))
    Right b → Tuple false (Just (CJ.encode codecB b))

-- | A codec for `List` values.
-- |
-- | Encodes as an array in JSON.
list ∷ ∀ a. CJ.Codec a → CJ.Codec (List.List a)
list codec = dimap Array.fromFoldable List.fromFoldable (CJ.named "List" (CJ.array codec))

-- | A codec for `NonEmptyList` values.
-- |
-- | Encodes as an array in JSON.
nonEmptyList ∷ ∀ a. CJ.Codec a → CJ.Codec (NEL.NonEmptyList a)
nonEmptyList codec = CJ.prismaticCodec "NonEmptyList" NEL.fromFoldable Array.fromFoldable (CJ.array codec)

-- | A codec for `Map` values.
-- |
-- | Encodes as an array of two-element key/value arrays in JSON.
map ∷ ∀ a b. Ord a ⇒ CJ.Codec a → CJ.Codec b → CJ.Codec (Map.Map a b)
map codecA codecB = dimap Map.toUnfoldable (Map.fromFoldable) (CJ.named "Map" (CJ.array (tuple codecA codecB)))

-- | A codec for `Map` values which have string keys.
-- |
-- | Encodes as an object in JSON.
strMap ∷ ∀ a. CJ.Codec a → CJ.Codec (Map.Map String a)
strMap codec =
  Codec.codec'
    (F.map Map.fromFoldable <<< traverse (traverse (Codec.decode codec)) <<< JO.entries <=< Codec.decode CJ.jobject)
    (CJ.encode CJ.jobject <<< JO.fromFoldableWithIndex <<< F.map (CJ.encode codec))

-- | A codec for `Set` values.
-- |
-- | Encodes as an array in JSON.
set ∷ ∀ a. Ord a ⇒ CJ.Codec a → CJ.Codec (Set.Set a)
set codec = dimap Array.fromFoldable Set.fromFoldable (CJ.named "Set" (CJ.array codec))

-- | A codec for `NonEmptySet` values.
-- |
-- | Encodes as an array in JSON.
nonEmptySet ∷ ∀ a. Ord a ⇒ CJ.Codec a → CJ.Codec (NESet.NonEmptySet a)
nonEmptySet codec = CJ.prismaticCodec "NonEmptySet" NESet.fromFoldable NESet.toUnfoldable (CJ.array codec)

-- | A codec for `Object` values.
-- |
-- | Encodes as an array of two-element key/value arrays in JSON.
foreignObject ∷ ∀ a. CJ.Codec a → CJ.Codec (Object.Object a)
foreignObject = dimap Object.toUnfoldable Object.fromFoldable <<< CJ.array <<< tuple CJ.string
