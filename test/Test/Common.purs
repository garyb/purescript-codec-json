module Test.Common where

import Prelude

import Control.Monad.Gen as Gen
import Control.Monad.Gen.Common as GenC
import Data.Codec.JSON.Common as CJC
import Data.Map.Gen (genMap)
import Data.String.Gen (genAsciiString)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object.Gen (genForeignObject)
import Test.QuickCheck (Result, quickCheck)
import Test.QuickCheck.Gen (Gen)
import Test.Util (genInt, propCodec)

main ∷ Effect Unit
main = do
  log "Checking Maybe codec"
  quickCheck propMaybeCodec

  log "Checking Either codec"
  quickCheck propEitherCodec

  log "Checking List codec"
  quickCheck propListCodec

  log "Checking Map codec"
  quickCheck propMapCodec

  log "Checking StrMap codec"
  quickCheck propStrMapCodec

  log "Checking Object codec"
  quickCheck propObjectCodec

propMaybeCodec ∷ Gen Result
propMaybeCodec =
  propCodec
    (GenC.genMaybe genInt)
    (CJC.maybe CJC.int)

propEitherCodec ∷ Gen Result
propEitherCodec =
  propCodec
    (GenC.genEither genInt genInt)
    (CJC.either CJC.int CJC.int)

propListCodec ∷ Gen Result
propListCodec =
  propCodec
    (Gen.unfoldable genInt)
    (CJC.list CJC.int)

propMapCodec ∷ Gen Result
propMapCodec =
  propCodec
    (genMap genInt genInt)
    (CJC.map CJC.int CJC.int)

propStrMapCodec ∷ Gen Result
propStrMapCodec =
  propCodec
    (genMap genAsciiString genInt)
    (CJC.strMap CJC.int)

propObjectCodec ∷ Gen Result
propObjectCodec =
  propCodec
    (genForeignObject genAsciiString genInt)
    (CJC.foreignObject CJC.int)
