module Data.Codec.JSON.Strict
  ( PropCodec
  , ClaimedProps
  , objectStrict
  , prop
  , record
  , recordProp
  , recordPropOptional
  ) where

import Prelude hiding ((<<<), (<=<), (>=>), (>>>))

import Codec.JSON.DecodeError as Error
import Control.Monad.Except (Except, lift, throwError, withExceptT)
import Control.Monad.State (StateT, modify_, runStateT)
import Data.Codec (Codec(..), codec, codec', decode, encode) as Codec
import Data.Codec.JSON as CJ
import Data.List ((:))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String as S
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import JSON (JObject, JSON)
import JSON.Object as JO
import JSON.Path as JP
import Prim.Row as Row
import Record.Unsafe as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | The set of properties that have been claimed "so far" during parsing of a
-- | record. This is used internally to track which properties have been parsed
-- | in order to determine which properties are unknown.
type ClaimedProps = Set.Set String

-- | Codec type for `JObject` prop/value pairs.
type PropCodec a =
  Codec.Codec
    (StateT ClaimedProps (Except Error.DecodeError))
    JObject
    (L.List (Tuple String JSON))
    a
    a

-- | A codec for objects that are encoded with specific properties. This codec
-- | will fail upon encountering unknown properties in the incoming record. Use
-- | `Data.Codec.JSON.object` for a version that ignores unknown properties.
-- |
-- | See also `Data.Codec.JSON.Record.objectStrict` for a more commonly useful
-- | version of this function.
objectStrict ∷ ∀ a. PropCodec a → CJ.Codec a
objectStrict codec = Codec.codec' dec enc
  where
  dec j = do
    obj ← Codec.decode CJ.jobject j
    Tuple rec claimedProps ← runStateT (Codec.decode codec obj) Set.empty

    let unclaimedProps = Set.difference (Set.fromFoldable (JO.keys obj)) claimedProps
    when (not Set.isEmpty unclaimedProps)
      $ throwError
      $ Error.error JP.Tip
      $
        "Unknown field(s): " <> S.joinWith ", " (Set.toUnfoldable unclaimedProps)

    pure rec

  enc a = Codec.encode CJ.jobject $ JO.fromFoldable $ Codec.encode codec a

-- | A codec for a property of an object.
prop ∷ ∀ a. String → CJ.Codec a → PropCodec a
prop key codec =
  Codec.codec
    ( \obj → do
        v ← case JO.lookup key obj of
          Just j → lift $ withExceptT liftError $ Codec.decode codec j
          Nothing → throwError (Error.noValueFound (JP.AtKey key JP.Tip))
        modify_ $ Set.insert key
        pure v
    )
    (\a → pure (Tuple key (CJ.encode codec a)))
  where
  liftError = Error.withPath (JP.AtKey key)

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
  ⇒ CJ.Codec a
  → PropCodec (Record r)
  → PropCodec (Record r')
recordProp codecA codecR = Codec.codec dec enc
  where
  key = reflectSymbol (Proxy @p)
  liftError = Error.withPath (JP.AtKey key)

  dec ∷ JObject → StateT _ (Except Error.DecodeError) (Record r')
  dec obj = do
    r ← Codec.decode codecR obj
    a ∷ a ← case JO.lookup key obj of
      Just val → lift $ withExceptT liftError $ Codec.decode codecA val
      Nothing → throwError $ Error.noValueFound (JP.AtKey key JP.Tip)
    modify_ $ Set.insert key
    pure $ Record.unsafeSet key a r

  enc ∷ Record r' → L.List (Tuple String JSON)
  enc val =
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
  ⇒ CJ.Codec a
  → PropCodec (Record r)
  → PropCodec (Record r')
recordPropOptional codecA codecR = Codec.codec dec enc
  where
  key = reflectSymbol (Proxy @p)
  liftError = Error.withPath (JP.AtKey key)

  dec ∷ JObject → StateT _ (Except Error.DecodeError) (Record r')
  dec obj = do
    r ← Codec.decode codecR obj
    a ∷ Maybe a ← case JO.lookup key obj of
      Just val → lift $ withExceptT liftError $ Just <$> Codec.decode codecA val
      Nothing → pure Nothing
    modify_ $ Set.insert key
    pure $ Record.unsafeSet key a r

  enc ∷ Record r' → L.List (Tuple String JSON)
  enc val = do
    let r = Codec.encode codecR ((unsafeCoerce ∷ Record r' → Record r) val)
    case Record.unsafeGet key val of
      Nothing → r
      Just val' → Tuple key (Codec.encode codecA val') : r
