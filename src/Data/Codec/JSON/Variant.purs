module Data.Codec.JSON.Variant where

import Prelude

import Codec.JSON.DecodeError as Error
import Control.Monad.Except (Except, except)
import Data.Array as Array
import Data.Codec (Codec(..))
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, inj, on)
import JSON (JSON)
import JSON.Object as JO
import Prim.Row as R
import Prim.RowList as RL
import Record as Record
import Type.Equality as TE
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Builds a codec for a variant from a record, similar to the way
-- | `Variant.match` works to pattern match on a variant.
-- |
-- | Commonly used to write decoders for sum-types, by providing a mapping from
-- | and to a Variant from that type and then using `dimap`.
-- |
-- | Each field in the record accepts an `Either`, where `Right` is used to
-- | specify a codec used for the constructor, and `Left` is used to specify a
-- | static value (generally as `Left unit` for nullary constructors).
-- |
-- | The variant will be encoded as a JSON object of the form
-- | `{ "tag": <name>, "value": <value> }`, where `<name>` is the name of the
-- | variant case, and `<value>` is the associated value (omitted in the case
-- | of static `Left`-defined values).
-- |
-- |```purescript
-- | codecMaybeMatch ∷ ∀ a. CJ.Codec a → CJ.Codec (Maybe a)
-- | codecMaybeMatch codecA =
-- |   dimap toVariant fromVariant
-- |     (CJV.variantMatch
-- |       { just: Right codecA
-- |       , nothing: Left unit
-- |       })
-- |   where
-- |   toVariant = case _ of
-- |     Just a → V.inj @"just" a
-- |     Nothing → V.inj @"nothing" unit
-- |   fromVariant = V.match
-- |     { just: Just
-- |     , nothing: \_ → Nothing
-- |     }
-- |```
variantMatch
  ∷ ∀ rl ri ro
  . RL.RowToList ri rl
  ⇒ VariantCodec rl ri ro
  ⇒ Record ri
  → CJ.Codec (Variant ro)
variantMatch = variantCodec (Proxy @rl)

-- | Builds codecs for variants in combination with `variantCase`.
-- |
-- | Provides an alternative means of building variant codecs to that of
-- | `variantMatch`, often for cases where the codec is being constructed
-- | with a fold or some other similar technique.
-- |
-- |```purescript
-- | codecMaybe ∷ ∀ a. CJ.Codec a → CJ.Codec (Maybe a)
-- | codecMaybe codecA =
-- |   dimap toVariant fromVariant
-- |     (CJV.variant
-- |       # CJV.variantCase _Just (Right codecA)
-- |       # CJV.variantCase _Nothing (Left unit))
-- |   where
-- |   toVariant = case _ of
-- |     Just a → V.inj _Just a
-- |     Nothing → V.inj _Nothing unit
-- |   fromVariant = V.case_
-- |     # V.on _Just Just
-- |     # V.on _Nothing (const Nothing)
-- |   _Just = Proxy @"just"
-- |   _Nothing = Proxy @"nothing"
-- |```
variant ∷ CJ.Codec (Variant ())
variant = Codec (\_ → except (Left (Error.basic "Unexpected value"))) case_

variantCase
  ∷ ∀ l a r r'
  . IsSymbol l
  ⇒ R.Cons l a r r'
  ⇒ Proxy l
  → Either a (CJ.Codec a)
  → CJ.Codec (Variant r)
  → CJ.Codec (Variant r')
variantCase proxy eacodec (Codec dec enc) = Codec.Codec dec' enc'
  where

  dec' ∷ JSON → Except CJ.DecodeError (Variant r')
  dec' j = do
    obj ← Codec.decode CJ.jobject j
    tag ← Codec.decode (CJ.prop "tag" CJ.string) obj
    if tag == reflectSymbol proxy then
      case eacodec of
        Left a → pure (inj proxy a)
        Right codec → do
          value ← Codec.decode (CJ.prop "value" CJ.json) obj
          inj proxy <$> Codec.decode codec value
    else
      coerceR <$> dec j

  enc' ∷ Variant r' → Tuple JSON (Variant r')
  enc' v =
    on proxy
      ( \v' → flip Tuple v
          $ Codec.encode CJ.jobject
          $ JO.fromEntries
          $ Array.catMaybes
              [ Just $ Tuple "tag" (Codec.encode CJ.string (reflectSymbol proxy))
              , Tuple "value" <<< (flip Codec.encode v') <$> hush eacodec
              ]
      )
      (\v' → enc v' $> v)
      v

  coerceR ∷ Variant r → Variant r'
  coerceR = unsafeCoerce

-- | The class used to enable the building of `Variant` codecs from a record of
-- | codecs.
class VariantCodec (rl ∷ RL.RowList Type) (ri ∷ Row Type) (ro ∷ Row Type) | rl → ri ro where
  variantCodec ∷ ∀ proxy. proxy rl → Record ri → CJ.Codec (Variant ro)

instance VariantCodec RL.Nil () () where
  variantCodec _ _ = variant

instance
  ( VariantCodec rs ri' ro'
  , R.Cons sym (Either a (CJ.Codec a)) ri' ri
  , R.Cons sym a ro' ro
  , IsSymbol sym
  , TE.TypeEquals co (Either a (CJ.Codec a))
  ) ⇒
  VariantCodec (RL.Cons sym co rs) ri ro where
  variantCodec _ codecs =
    variantCase (Proxy @sym) codec tail
    where
    codec ∷ Either a (CJ.Codec a)
    codec = TE.from (Record.get (Proxy @sym) codecs)

    tail ∷ CJ.Codec (Variant ro')
    tail = variantCodec (Proxy @rs) ((unsafeCoerce ∷ Record ri → Record ri') codecs)
