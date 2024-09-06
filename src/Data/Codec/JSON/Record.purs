module Data.Codec.JSON.Record where

import Data.Codec.JSON as CJ
import Data.Codec.JSON.Strict as CJS
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol)
import Prim.Row as R
import Prim.RowList as RL
import Record as Rec
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Constructs a `Codec` for a `Record` from a record of codecs.
-- |
-- | ```purescript
-- | type Person = { name ∷ String, age ∷ Int }
-- |
-- | personCodec ∷ CJ.Codec Person
-- | personCodec = CAR.object { name: CJ.string, age: CJ.int }
-- | ```
object
  ∷ ∀ ri ro rl
  . RL.RowToList ri rl
  ⇒ RowListCodec rl ri ro
  ⇒ Record ri
  → CJ.Codec (Record ro)
object rec = CJ.object (record rec)

-- | A version of the `object` function that fails upon encountering unknown
-- | properties while decoding a record.
-- |
-- | ```purescript
-- | type Person = { name ∷ String, age ∷ Int }
-- |
-- | personCodec ∷ CJ.Codec Person
-- | personCodec = CAR.objectStrict { name: CJ.string, age: CJ.int }
-- | ```
objectStrict
  ∷ ∀ ri ro rl
  . RL.RowToList ri rl
  ⇒ RowListCodecStrict rl ri ro
  ⇒ Record ri
  → CJ.Codec (Record ro)
objectStrict rec = CJS.objectStrict (recordStrict rec)

-- | Constructs a `JPropCodec` for a `Record` from a record of codecs. Commonly
-- | the `object` function in this module will be the preferred choice, as that
-- | produces a `Codec` instead.
record
  ∷ ∀ ri ro rl
  . RL.RowToList ri rl
  ⇒ RowListCodec rl ri ro
  ⇒ Record ri
  → CJ.PropCodec (Record ro)
record = rowListCodec @rl

-- | Constructs a `JPropCodec` for a `Record` from a record of codecs. Commonly
-- | the `object` function in this module will be the preferred choice, as that
-- | produces a `Codec` instead.
recordStrict
  ∷ ∀ ri ro rl
  . RL.RowToList ri rl
  ⇒ RowListCodecStrict rl ri ro
  ⇒ Record ri
  → CJS.PropCodec (Record ro)
recordStrict = rowListCodecStrict @rl

-- | Used to wrap codec values provided in `record` to indicate the field is optional.
-- |
-- | This will only decode the property as `Nothing` if the field does not exist
-- | in the object - having a values such as `null` assigned will need handling
-- | separately.
-- |
-- | The property will be omitted when encoding and the value is `Nothing`.
newtype Optional a = Optional (CJ.Codec a)

-- | A lowercase alias for `Optional`, provided for stylistic reasons only.
optional ∷ ∀ a. CJ.Codec a → Optional a
optional = Optional

-- | The class used to enable the building of `Record` codecs by providing a
-- | record of codecs.
class RowListCodec (rl ∷ RL.RowList Type) (ri ∷ Row Type) (ro ∷ Row Type) | rl → ri ro where
  rowListCodec ∷ Record ri → CJ.PropCodec (Record ro)

instance RowListCodec RL.Nil () () where
  rowListCodec _ = CJ.record

instance
  ( RowListCodec rs ri' ro'
  , R.Cons sym (Optional a) ri' ri
  , R.Cons sym (Maybe a) ro' ro
  , IsSymbol sym
  ) ⇒
  RowListCodec (RL.Cons sym (Optional a) rs) ri ro where
  rowListCodec codecs =
    CJ.recordPropOptional @sym codec tail
    where
    codec ∷ CJ.Codec a
    codec = coerce (Rec.get (Proxy @sym) codecs ∷ Optional a)

    tail ∷ CJ.PropCodec (Record ro')
    tail = rowListCodec @rs ((unsafeCoerce ∷ Record ri → Record ri') codecs)

else instance
  ( RowListCodec rs ri' ro'
  , R.Cons sym (CJ.Codec a) ri' ri
  , R.Cons sym a ro' ro
  , IsSymbol sym
  ) ⇒
  RowListCodec (RL.Cons sym (CJ.Codec a) rs) ri ro where
  rowListCodec codecs =
    CJ.recordProp @sym codec tail
    where
    codec ∷ CJ.Codec a
    codec = Rec.get (Proxy @sym) codecs

    tail ∷ CJ.PropCodec (Record ro')
    tail = rowListCodec @rs ((unsafeCoerce ∷ Record ri → Record ri') codecs)

-- | The class used to enable the building of `Record` codecs by providing a
-- | record of codecs.
class RowListCodecStrict (rl ∷ RL.RowList Type) (ri ∷ Row Type) (ro ∷ Row Type) | rl → ri ro where
  rowListCodecStrict ∷ Record ri → CJS.PropCodec (Record ro)

instance RowListCodecStrict RL.Nil () () where
  rowListCodecStrict _ = CJS.record

instance
  ( RowListCodecStrict rs ri' ro'
  , R.Cons sym (Optional a) ri' ri
  , R.Cons sym (Maybe a) ro' ro
  , IsSymbol sym
  ) ⇒
  RowListCodecStrict (RL.Cons sym (Optional a) rs) ri ro where
  rowListCodecStrict codecs =
    CJS.recordPropOptional @sym codec tail
    where
    codec ∷ CJ.Codec a
    codec = coerce (Rec.get (Proxy @sym) codecs ∷ Optional a)

    tail ∷ CJS.PropCodec (Record ro')
    tail = rowListCodecStrict @rs ((unsafeCoerce ∷ Record ri → Record ri') codecs)

else instance
  ( RowListCodecStrict rs ri' ro'
  , R.Cons sym (CJ.Codec a) ri' ri
  , R.Cons sym a ro' ro
  , IsSymbol sym
  ) ⇒
  RowListCodecStrict (RL.Cons sym (CJ.Codec a) rs) ri ro where
  rowListCodecStrict codecs =
    CJS.recordProp @sym codec tail
    where
    codec ∷ CJ.Codec a
    codec = Rec.get (Proxy @sym) codecs

    tail ∷ CJS.PropCodec (Record ro')
    tail = rowListCodecStrict @rs ((unsafeCoerce ∷ Record ri → Record ri') codecs)
