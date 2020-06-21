module Test.Main where

import Timeline.Time.Unit (DecidedUnit)
import Timeline.Time.Value (DecidedValue)
import Timeline.Time.Min (DecidedMin)
import Timeline.Time.Max (DecidedMax)
import Timeline.Time.Span (DecidedSpan)
import Timeline.Time.Bounds (DecidedBounds)
import Timeline.Time.Limit (DecidedLimit)
import Timeline.Time.MaybeLimit (DecidedMaybeLimit)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer, class DecodeArrayBuffer, class DynamicByteLength
  , encodeArrayBuffer, decodeArrayBuffer)
import Data.ArrayBuffer.Class.Types (Float64BE (..))
import Data.Identity (Identity)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck (class Arbitrary, arbitrary, quickCheck, Result (..))
import Test.Spec (describe, it, SpecT)
import Test.Spec.Runner (runSpec', defaultConfig)
import Test.Spec.Reporter.Console (consoleReporter)
import Type.Proxy (Proxy (..))


main :: Effect Unit
main = launchAff_ $ runSpec' (defaultConfig {timeout = Nothing}) [consoleReporter] tests

tests :: SpecT Aff Unit Identity Unit
tests = do
  describe "Json" do
    jsonTest "DecidedUnit" (Proxy :: Proxy DecidedUnit)
    jsonTest "DecidedValue" (Proxy :: Proxy DecidedValue)
    jsonTest "DecidedMin" (Proxy :: Proxy DecidedMin)
    jsonTest "DecidedMax" (Proxy :: Proxy DecidedMax)
    jsonTest "DecidedSpan" (Proxy :: Proxy DecidedSpan)
    jsonTest "DecidedBounds" (Proxy :: Proxy DecidedBounds)
    jsonTest "DecidedLimit" (Proxy :: Proxy DecidedLimit)
    jsonTest "DecidedMaybeLimit" (Proxy :: Proxy DecidedMaybeLimit)
  describe "Binary" do
    binaryTest "DecidedUnit" (Proxy :: Proxy DecidedUnit)
    -- binaryTest "DecidedValue" (Proxy :: Proxy DecidedValue)
    -- binaryTest "DecidedMin" (Proxy :: Proxy DecidedMin)
    -- binaryTest "DecidedMax" (Proxy :: Proxy DecidedMax)
    binaryTest "DecidedSpan" (Proxy :: Proxy DecidedSpan)
    binaryTest "DecidedBounds" (Proxy :: Proxy DecidedBounds)
    -- binaryTest "DecidedLimit" (Proxy :: Proxy DecidedLimit)
    -- binaryTest "DecidedMaybeLimit" (Proxy :: Proxy DecidedMaybeLimit)
  where
    jsonTest :: forall a
              . Arbitrary a
             => Show a
             => Eq a
             => EncodeJson a
             => DecodeJson a
             => String -> Proxy a -> _
    jsonTest name proxy = it name (liftEffect (quickCheck (jsonIso proxy)))
    binaryTest :: forall a
                . Arbitrary a
               => Show a
               => Eq a
               => EncodeArrayBuffer a
               => DecodeArrayBuffer a
               => DynamicByteLength a
               => String -> Proxy a -> _
    binaryTest name proxy = it name (liftEffect (quickCheck (binaryIso proxy)))


jsonIso :: forall a
         . Eq a
        => Show a
        => EncodeJson a
        => DecodeJson a
        => Proxy a -> a -> Result
jsonIso Proxy x =
  -- trace x \_ ->
  let result = decodeJson (encodeJson x)
  in  case result of
        Left e -> Failed $ "Couldn't parse: " <> e
        Right y
          | y == x -> Success
          | otherwise ->
              Failed $ "Not equal - original " <> show (show x) <> "\nresult: " <> show (show y)


binaryIso :: forall a
           . Eq a
          => Show a
          => EncodeArrayBuffer a
          => DecodeArrayBuffer a
          => DynamicByteLength a
          => Proxy a -> a -> Result
binaryIso Proxy x = unsafePerformEffect do
  buf <- encodeArrayBuffer x
  mY <- decodeArrayBuffer buf
  pure $
    if mY == Just x
      then Success
      else Failed $ "Not equal - original " <> show (show x) <> "\nresult: " <> show (show mY)


newtype BinaryFloat = BinaryFloat Float64BE
derive instance genericBinaryFloat :: Generic BinaryFloat _
derive newtype instance eqBinaryFloat :: Eq BinaryFloat
derive newtype instance ordBinaryFloat :: Ord BinaryFloat
derive newtype instance showBinaryFloat :: Show BinaryFloat
derive newtype instance encodeArrayBufferBinaryFloat :: EncodeArrayBuffer BinaryFloat
derive newtype instance decodeArrayBufferBinaryFloat :: DecodeArrayBuffer BinaryFloat
derive newtype instance dynamicByteLengthBinaryFloat :: DynamicByteLength BinaryFloat
instance arbitraryBinaryFloat :: Arbitrary BinaryFloat where
  arbitrary = BinaryFloat <<< Float64BE <$> arbitrary
