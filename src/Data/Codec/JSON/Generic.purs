module Data.Codec.JSON.Generic where

import Prelude

import Codec.JSON.DecodeError as Error
import Control.Alt ((<|>))
import Control.Monad.Except (except)
import Data.Bifunctor (lmap)
import Data.Codec as C
import Data.Codec.JSON as CJ
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), from, to)
import Data.Symbol (class IsSymbol, reflectSymbol)
import JSON (JSON)
import JSON as J
import JSON.Path as JP
import Type.Proxy (Proxy(..))

-- | Encodes nullary sums with a `Generic` instance as strings that match the constructor names.
-- |
-- | ```purescript
-- | import JSON as J
-- | import Data.Codec.JSON as CJ
-- | import Data.Codec.JSON.Generic as CJG
-- |
-- | data MySum = Ctor1 | Ctor2 | Ctor3
-- | derive instance Generic MySum _
-- |
-- | CJ.encode (CJG.nullarySum "MySum") Ctor1 == J.fromString "Ctor1"
-- | CJ.decode (CJG.nullarySum "MySum") (J.fromString "Ctor3") == Right Ctor3
-- |```
nullarySum ∷ ∀ a r. Generic a r ⇒ NullarySumCodec r ⇒ String → CJ.Codec a
nullarySum name =
  C.codec'
    (except <<< map to <<< nullarySumDecode name)
    (nullarySumEncode <<< from)

class NullarySumCodec r where
  nullarySumEncode ∷ r → JSON
  nullarySumDecode ∷ String → JSON → Either CJ.DecodeError r

instance (NullarySumCodec a, NullarySumCodec b) ⇒ NullarySumCodec (Sum a b) where
  nullarySumEncode = case _ of
    Inl a → nullarySumEncode a
    Inr b → nullarySumEncode b
  nullarySumDecode name j = Inl <$> nullarySumDecode name j
    <|> Inr <$> nullarySumDecode name j

instance IsSymbol name ⇒ NullarySumCodec (Constructor name NoArguments) where
  nullarySumEncode _ =
    J.fromString $ reflectSymbol (Proxy @name)
  nullarySumDecode name j =
    lmap (Error.withContext ("Could not decode " <> name)) do
      tag ← CJ.decode CJ.string j
      if tag /= reflectSymbol (Proxy @name) then
        Left (Error.error (JP.AtKey "tag" JP.Tip) ("Unexpected value '" <> tag <> "' found"))
      else
        Right (Constructor NoArguments)
