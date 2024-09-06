module Data.Codec.JSON
  ( Codec
  , encode
  , decode
  , json
  , null
  , boolean
  , number
  , int
  , string
  , codePoint
  , char
  , jarray
  , jobject
  , void
  , array
  , IndexedCodec
  , indexedArray
  , index
  , PropCodec
  , object
  , prop
  , record
  , recordProp
  , recordPropOptional
  , nullable
  , named
  , coercible
  , prismaticCodec
  , module Exports
  ) where

import Prelude hiding ((<<<), (<=<), (>=>), (>>>))

import Codec.JSON.DecodeError (DecodeError(..))
import Codec.JSON.DecodeError (DecodeError(..)) as Exports
import Codec.JSON.DecodeError as Error
import Control.Monad.Except (Except, except, runExcept)
import Data.Codec (Codec(..), Codec', codec, codec', decode, encode) as Codec
import Data.Codec (fix, hoist, identity, (<~<), (>~>), (~)) as Exports
import Data.Either (Either(..))
import Data.Int as I
import Data.List ((:))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import JSON (JArray, JObject, JSON)
import JSON as J
import JSON.Array as JA
import JSON.Object as JO
import JSON.Path as JP
import Prim.Coerce (class Coercible)
import Prim.Row as Row
import Record.Unsafe as Record
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Codec type for `Json` values.
type Codec a = Codec.Codec' (Except DecodeError) JSON a

-- | Encodes a value as JSON using the specified code.
encode ∷ ∀ a b c d. Codec.Codec (Except DecodeError) a b c d → c → b
encode = Codec.encode

-- | Tries to decode JSON to a value using the specified code.
decode ∷ ∀ a b c d. Codec.Codec (Except DecodeError) a b c d → a → Either DecodeError d
decode codec j = runExcept (Codec.decode codec j)

-- | The "identity codec" for `Json` values.
json ∷ Codec JSON
json = Codec.codec' pure identity

jsonPrimCodec ∷ ∀ a. String → (JSON → Maybe a) → (a → JSON) → Codec a
jsonPrimCodec ty f =
  Codec.codec' \j →
    except case f j of
      Just a → Right a
      Nothing → Left
        ( DecodeError
            { path: JP.Tip
            , message: "Expected value of type " <> ty
            , causes: []
            }
        )

-- | A codec for `null` values in `Json`.
null ∷ Codec Unit
null = jsonPrimCodec "Null" J.toNull (const J.null)

-- | A codec for `Boolean` values in `Json`.
boolean ∷ Codec Boolean
boolean = jsonPrimCodec "Boolean" J.toBoolean J.fromBoolean

-- | A codec for `Number` values in `Json`.
number ∷ Codec Number
number = jsonPrimCodec "Number" J.toNumber J.fromNumber

-- | A codec for `Int` values in `Json`.
int ∷ Codec Int
int = jsonPrimCodec "Int" (\j → I.fromNumber =<< J.toNumber j) (\n → J.fromNumber (I.toNumber n))

-- | A codec for `String` values in `Json`.
string ∷ Codec String
string = jsonPrimCodec "String" J.toString J.fromString

-- | A codec for `Codepoint` values in `Json`.
codePoint ∷ Codec S.CodePoint
codePoint = jsonPrimCodec "CodePoint" (\j → S.codePointAt 0 =<< J.toString j) (\cp → J.fromString (S.singleton cp))

-- | A codec for `Char` values in `Json`.
char ∷ Codec Char
char = jsonPrimCodec "Char" (\j → SCU.toChar =<< J.toString j) (\c → J.fromString (SCU.singleton c))

-- | A codec for `Void` values.
void ∷ Codec Void
void = jsonPrimCodec "Void" (const Nothing) absurd

-- | A codec for `JArray` values in `Json`. This does not decode the values of the array, for that
-- | use `array` for a general array decoder, or `indexedArray` with `index` to decode fixed length
-- | array encodings.
jarray ∷ Codec JArray
jarray = jsonPrimCodec "Array" J.toJArray J.fromJArray

-- | A codec for `JObject` values in `Json`.
jobject ∷ Codec JObject
jobject = jsonPrimCodec "Object" J.toJObject J.fromJObject

-- | A codec for arbitrary length `Array`s where every item in the array
-- | shares the same type.
-- |
-- | ``` purescript
-- | import Data.Codec.JSON as CJ
-- |
-- | codecIntArray ∷ CJ.Codec (Array Int)
-- | codecIntArray = CJ.array CJ.int
-- | ```
array ∷ ∀ a. Codec a → Codec (Array a)
array codec =
  Codec.codec'
    ( \j → do
        arr ← Codec.decode jarray j
        traverseWithIndex
          ( \ix a →
              except case decode codec a of
                Left err → Left (Error.withPath (JP.AtIndex ix) err)
                value → value
          )
          (JA.toArray arr)
    )
    (\a → J.fromArray (map (encode codec) a))

-- | Codec type for specifically indexed `JArray` elements.
type IndexedCodec a =
  Codec.Codec
    (Except DecodeError)
    JArray
    (L.List JSON)
    a
    a

-- | A codec for types that are encoded as an array with a specific layout.
-- |
-- | For example, if we'd like to encode a `Person` as a 2-element array, like
-- | `["Rashida", 37]`, we could write the following codec:
-- |
-- | ```purescript
-- | import Data.Codec.JSON ((~))
-- | import Data.Codec.JSON as CJ
-- |
-- | type Person = { name ∷ String, age ∷ Int }
-- |
-- | codecPerson ∷ CJ.Codec Person
-- | codecPerson = CJ.indexedArray $
-- |   { name: _, age: _ }
-- |     <$> _.name ~ CJ.index 0 CJ.string
-- |     <*> _.age ~ CJ.index 1 CJ.int
-- | ```
indexedArray ∷ ∀ a. IndexedCodec a → Codec a
indexedArray codec =
  Codec.codec'
    (\j → Codec.decode codec =<< Codec.decode jarray j)
    (\a → encode jarray (JA.fromFoldable (encode codec a)))

-- | A codec for an item in an `indexedArray`.
index ∷ ∀ a. Int → Codec a → IndexedCodec a
index ix codec =
  Codec.codec
    ( \xs →
        except case JA.index ix xs of
          Just j →
            case decode codec j of
              Left err → Left (Error.withPath (JP.AtIndex ix) err)
              value → value
          Nothing →
            Left (Error.noValueFound (JP.AtIndex ix JP.Tip))
    )
    (\a → pure (encode codec a))

-- | Codec type for `JObject` prop/value pairs.
type PropCodec a =
  Codec.Codec
    (Except DecodeError)
    JObject
    (L.List (Tuple String JSON))
    a
    a

-- | A codec for objects that are encoded with specific properties. This codec
-- | will ignore any unknown properties in the incoming record. Use
-- | `Data.Codec.JSON.Strict.objectStrict` for a version that fails upon
-- | encountering unknown properties.
-- |
-- | See also `Data.Codec.JSON.Record.object` for a more commonly useful version
-- | of this function.
object ∷ ∀ a. PropCodec a → Codec a
object codec =
  Codec.codec'
    (\j → Codec.decode codec =<< Codec.decode jobject j)
    (\a → encode jobject (JO.fromFoldable (encode codec a)))

-- | A codec for a property of an object.
prop ∷ ∀ a. String → Codec a → PropCodec a
prop key codec =
  Codec.codec
    ( \obj →
        except case JO.lookup key obj of
          Just j →
            case decode codec j of
              Left err → Left (Error.withPath (JP.AtKey key) err)
              value → value
          Nothing →
            Left (Error.noValueFound (JP.AtKey key JP.Tip))
    )
    (\a → pure (Tuple key (encode codec a)))

-- | The starting value for a object-record codec. Used with `recordProp` it
-- | provides a convenient method for defining codecs for record types that
-- | encode into JSON objects of the same shape.
-- |
-- | For example, to encode a record as the JSON object
-- | `{ "name": "Karl", "age": 25 }` we would define a codec like this:
-- | ```
-- | import Data.Codec.JSON as CJ
-- |
-- | type Person = { name ∷ String, age ∷ Int }
-- |
-- | codecPerson ∷ CJ.Codec Person
-- | codecPerson =
-- |   CJ.object $ CJ.record
-- |     # CJ.recordProp @"name" CJ.string
-- |     # CJ.recordProp @"age" CJ.int
-- | ```
-- |
-- | See also `Data.Codec.JSON.Record.object` for a more commonly useful
-- | version of this function.
record ∷ PropCodec {}
record = Codec.Codec (const (pure {})) pure

-- | Used with `record` to define codecs for record types that encode into JSON
-- | objects of the same shape. See the comment on `record` for an example.
recordProp
  ∷ ∀ @p a r r'
  . IsSymbol p
  ⇒ Row.Cons p a r r'
  ⇒ Codec a
  → PropCodec (Record r)
  → PropCodec (Record r')
recordProp codecA codecR =
  let key = reflectSymbol (Proxy @p) in Codec.codec (dec' key) (enc' key)
  where
  dec' ∷ String → JObject → Except DecodeError (Record r')
  dec' key obj = do
    r ← Codec.decode codecR obj
    a ← except case JO.lookup key obj of
      Just val →
        case decode codecA val of
          Left err → Left (Error.withPath (JP.AtKey key) err)
          value → value
      Nothing →
        Left (Error.noValueFound (JP.AtKey key JP.Tip))
    pure $ Record.unsafeSet key a r

  enc' ∷ String → Record r' → L.List (Tuple String JSON)
  enc' key val =
    Tuple key (Codec.encode codecA (Record.unsafeGet key val))
      : Codec.encode codecR ((unsafeCoerce ∷ Record r' → Record r) val)

-- | Used with `record` to define an optional field.
-- |
-- | This will only decode the property as `Nothing` if the field does not exist
-- | in the object - having a values such as `null` assigned will need handling
-- | separately.
-- |
-- | The property will be omitted when encoding and the value is `Nothing`.
recordPropOptional
  ∷ ∀ @p a r r'
  . IsSymbol p
  ⇒ Row.Cons p (Maybe a) r r'
  ⇒ Codec a
  → PropCodec (Record r)
  → PropCodec (Record r')
recordPropOptional codecA codecR = Codec.codec dec' enc'
  where
  key ∷ String
  key = reflectSymbol (Proxy @p)

  dec' ∷ JObject → Except DecodeError (Record r')
  dec' obj = do
    r ← Codec.decode codecR obj
    a ← except case JO.lookup key obj of
      Just val →
        case decode codecA val of
          Left err → Left (Error.withPath (JP.AtKey key) err)
          value → Just <$> value
      _ →
        Right Nothing
    pure $ Record.unsafeSet key a r

  enc' ∷ Record r' → L.List (Tuple String JSON)
  enc' val = do
    let w = Codec.encode codecR ((unsafeCoerce ∷ Record r' → Record r) val)
    case Record.unsafeGet key val of
      Just a → Tuple key (Codec.encode codecA a) : w
      Nothing → w

-- | A codec for JSON values that can be `null` or some other value.
-- |
-- | This should not be used if an accurate representation of nested `Maybe` values is required, as
-- | values like `Just Nothing` cannot be encoded. For nested `Maybe`s consider using
-- | `Data.Codec.JSON.Common.maybe` instead.
nullable ∷ ∀ a. Codec a → Codec (Maybe a)
nullable codec =
  Codec.codec'
    ( \j → except case decode codec j of
        Left err1 →
          case decode null j of
            Left err2 → Left (err1 <> err2)
            Right _ → Right Nothing
        Right value →
          Right (Just value)
    )
    case _ of
      Just a → encode codec a
      Nothing → J.null

-- | A codec for introducing names into error messages - useful when definiting a codec for a type
-- | synonym for a record, for instance.
named ∷ ∀ a. String → Codec a → Codec a
named name codec =
  Codec.codec'
    ( \j →
        except case decode codec j of
          Left err → Left (Error.withContext ("Could not decode " <> name) err)
          value → value
    )
    (encode codec)

-- | A codec for types that can be safely coerced.
-- |
-- | Accepts the name of the target type as an argument to improve error messaging when the inner
-- | codec fails.
coercible ∷ ∀ a b. Coercible a b ⇒ String → Codec a → Codec b
coercible name codec =
  Codec.codec'
    ( \j →
        except case decode codec j of
          Left err → Left (Error.withContext ("Could not decode " <> name) err)
          value → coerce value
    )
    (coerce (encode codec))

-- | Adapts an existing codec with a pair of functions to allow a value to be
-- | further refined. If the inner decoder fails an `UnexpectedValue` error will
-- | be raised for JSON input.
-- |
-- | This function is named as such as the pair of functions it accepts
-- | correspond with the `preview` and `review` functions of a `Prism`-style lens.
-- |
-- | An example of this would be a codec for `Data.String.NonEmpty.NonEmptyString`:
-- |
-- | ```purescript
-- | nonEmptyString ∷ CJ.Codec NES.NonEmptyString
-- | nonEmptyString = CJ.prismaticCodec "NonEmptyString" NES.fromString NES.toString CJ.string
-- | ```
-- |
-- | Another example might be to handle a mapping from a small sum type to
-- | strings:
-- |
-- | ```purescript
-- | data Direction = North | South | West | East
-- |
-- | directionCodec :: Codec Direction
-- | directionCodec = CJ.prismaticCodec "Direction" dec enc string
-- |   where
-- |     dec = case _ of
-- |       "N" -> Just North
-- |       "S" -> Just South
-- |       "W" -> Just West
-- |       "E" -> Just East
-- |       _ -> Nothing
-- |
-- |     enc = case _ of
-- |       North -> "N"
-- |       South -> "S"
-- |       West -> "W"
-- |       East -> "E"
-- | ```
-- |
-- | Although for this latter case there are some other options too, in the form
-- | of `Data.Codec.JSON.Generic.nullarySum` and `Data.Codec.JSON.Sum.enumSum`.
prismaticCodec ∷ ∀ a b. String → (a → Maybe b) → (b → a) → Codec a → Codec b
prismaticCodec name f g codec =
  Codec.codec'
    ( \j →
        except $ case decode codec j of
          Left err →
            Left (Error.withContext ("Could not decode " <> name) err)
          Right a →
            case f a of
              Just b →
                Right b
              Nothing →
                Left (Error.basic ("Could not decode " <> name <> ", unexpected value found"))
    )
    (\b → encode codec (g b))
