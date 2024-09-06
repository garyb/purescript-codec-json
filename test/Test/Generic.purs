module Test.Generic where

import Prelude

import Data.Codec.JSON.Generic as CJG
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (genericArbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.Util (propCodec)

data MySum = Ctor1 | Ctor2 | MoarCtors

derive instance Eq MySum
derive instance Generic MySum _

instance Show MySum where
  show = genericShow

genMySum ∷ Gen MySum
genMySum = genericArbitrary

main ∷ Effect Unit
main = do
  log "Check nullarySum"
  quickCheck (propCodec genMySum (CJG.nullarySum "MySum"))
