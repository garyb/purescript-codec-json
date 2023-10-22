module Test.Util where

import Prelude

import Codec.JSON.DecodeError as DecodeError
import Control.Monad.Gen as Gen
import Data.Codec.JSON as CJ
import Data.Either (Either(..))
import Test.QuickCheck (Result(..), (<?>))
import Test.QuickCheck.Gen (Gen)

propCodec' ∷ ∀ a. (a → a → Boolean) → (a → String) → Gen a → CJ.Codec a → Gen Result
propCodec' eq' show' gen codec = do
  x ← gen
  pure case CJ.decode codec (CJ.encode codec x) of
    Left err →
      Failed
        $ "Decoding " <> show' x <> " failed with error: " <> DecodeError.print err
    Right y →
      x `eq'` y <?> "Decoded result:\n" <> show' x <> "\n\nDid not match input:\n" <> show' y

propCodec ∷ ∀ a. Eq a ⇒ Show a ⇒ Gen a → CJ.Codec a → Gen Result
propCodec = propCodec' eq show

propCodec'' ∷ ∀ a. Eq a ⇒ (a → String) → Gen a → CJ.Codec a → Gen Result
propCodec'' = propCodec' eq

genInt ∷ Gen Int
genInt = Gen.chooseInt (-100000) 100000
