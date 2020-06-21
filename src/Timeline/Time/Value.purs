module Timeline.Time.Value where

import Prelude
import Data.Maybe (Maybe (..))
import Data.UInt (fromInt) as UInt
import Data.NonEmpty (NonEmpty(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , decodeJson
  , (~>)
  , jsonEmptyObject
  , (:=)
  , (.:)
  )
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer
  , class DecodeArrayBuffer
  , class DynamicByteLength
  , putArrayBuffer
  , readArrayBuffer
  , byteLength
  )
import Data.ArrayBuffer.Class.Types (Uint8 (..), Float64BE (..))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)

data DecidedValue
  = DecidedValueNumber Number

-- | Creates a string out of a `DecidedValue`, to be used in a text field.
intermediaryDecidedValue :: DecidedValue -> String
intermediaryDecidedValue v = case v of
  DecidedValueNumber n -> show n

-- FIXME date picker?
derive instance genericDecidedValue :: Generic DecidedValue _

instance eqDecidedValue :: Eq DecidedValue where
  eq = genericEq

instance showDecidedValue :: Show DecidedValue where
  show = genericShow

instance encodeJsonDecidedValue :: EncodeJson DecidedValue where
  encodeJson x = case x of
    DecidedValueNumber y -> "numberValue" := y ~> jsonEmptyObject

instance decodeJsonDecidedValue :: DecodeJson DecidedValue where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = DecidedValueNumber <$> o .: "numberValue"
    decodeNumber

instance encodeArrayBufferDecidedValue :: EncodeArrayBuffer DecidedValue where
  putArrayBuffer b o x = case x of
    DecidedValueNumber y -> do
      mW <- putArrayBuffer b o (Uint8 (UInt.fromInt 0))
      case mW of
        Nothing -> pure Nothing
        Just w -> do
          mW' <- putArrayBuffer b (o + w) (Float64BE y)
          case mW' of
            Nothing -> pure (Just w)
            Just w' -> pure (Just (w + w'))

instance decodeArrayBufferDecidedValue :: DecodeArrayBuffer DecidedValue where
  readArrayBuffer b o = do
    mC <- readArrayBuffer b o
    case mC of
      Nothing -> pure Nothing
      Just (Uint8 c) -> case unit of
        _ | c == UInt.fromInt 0 ->
            map (\(Float64BE y) -> DecidedValueNumber y) <$> readArrayBuffer b (o + 1)
          | otherwise -> pure Nothing

instance dynamicByteLengthDecidedValue :: DynamicByteLength DecidedValue where
  byteLength x = (_ + 1) <$> case x of
    DecidedValueNumber y -> byteLength (Float64BE y)

instance arbitraryDecidedValue :: Arbitrary DecidedValue where
  arbitrary = oneOf $ NonEmpty (DecidedValueNumber <$> arbitrary) []
