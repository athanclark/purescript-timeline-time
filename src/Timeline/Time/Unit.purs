module Timeline.Time.Unit where

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
  , encodeJson
  , decodeJson
  , fail
  )
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer
  , class DecodeArrayBuffer
  , class DynamicByteLength
  , putArrayBuffer
  , readArrayBuffer
  )
import Data.ArrayBuffer.Class.Types (Uint8 (..))
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (elements)

data DecidedUnit
  = DecidedUnitNumber
  | DecidedUnitFoo

derive instance genericDecidedUnit :: Generic DecidedUnit _

instance eqDecidedUnit :: Eq DecidedUnit where
  eq = genericEq

instance showDecidedUnit :: Show DecidedUnit where
  show = genericShow

instance encodeJsonDecidedUnit :: EncodeJson DecidedUnit where
  encodeJson x = case x of
    DecidedUnitNumber -> encodeJson "number"
    DecidedUnitFoo -> encodeJson "foo"

instance decodeJsonDecidedUnit :: DecodeJson DecidedUnit where
  decodeJson json = do
    s <- decodeJson json
    case s of
      "number" -> pure DecidedUnitNumber
      "foo" -> pure DecidedUnitFoo
      _ -> fail $ "Unrecognized DecidedUnit: " <> s

instance encodeArrayBufferDecidedUnit :: EncodeArrayBuffer DecidedUnit where
  putArrayBuffer b o x = putArrayBuffer b o $ Uint8 $ UInt.fromInt $ case x of
    DecidedUnitNumber -> 0
    DecidedUnitFoo -> 1

instance decodeArrayBufferDecidedUnit :: DecodeArrayBuffer DecidedUnit where
  readArrayBuffer b o = do
    mC <- readArrayBuffer b o
    case mC of
      Nothing -> pure Nothing
      Just (Uint8 c) -> pure $ case unit of
        _ | c == UInt.fromInt 0 -> Just DecidedUnitNumber
          | c == UInt.fromInt 1 -> Just DecidedUnitFoo
          | otherwise -> Nothing

instance dynamicByteLengthDecidedUnit :: DynamicByteLength DecidedUnit where
  byteLength _ = pure 1

instance arbitraryDecidedUnit :: Arbitrary DecidedUnit where
  arbitrary = elements $ NonEmpty DecidedUnitNumber [ DecidedUnitFoo ]
