module Timeline.Time.Max where

import Timeline.Time.Value (DecidedValue(..))
import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either)
import Data.UInt (fromInt) as UInt
import Data.NonEmpty (NonEmpty(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , decodeJson
  , Json
  , (~>)
  , jsonEmptyObject
  , (:=)
  , (.:)
  , JsonDecodeError
  )
import Data.ArrayBuffer.Types (ArrayBuffer, ByteOffset, ByteLength)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer
  , class DecodeArrayBuffer
  , class DynamicByteLength
  , putArrayBuffer
  , readArrayBuffer
  , byteLength
  )
import Data.ArrayBuffer.Class.Types (Uint8 (..), Float64BE (..))
import Effect (Effect)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, oneOf)
import Type.Proxy (Proxy(..))

type Max a
  = { end :: a }

genMax :: forall a. Arbitrary a => Proxy a -> Gen (Max a)
genMax Proxy = do
  end <- arbitrary
  pure { end }

encodeJsonMax :: forall a. EncodeJson a => Proxy a -> Max a -> Json
encodeJsonMax Proxy { end } = "end" := end ~> jsonEmptyObject

decodeJsonMax :: forall a. DecodeJson a => Proxy a -> Json -> Either JsonDecodeError (Max a)
decodeJsonMax Proxy json = do
  o <- decodeJson json
  end <- o .: "end"
  pure { end }

putArrayBufferMax :: forall a. EncodeArrayBuffer a => Proxy a -> ArrayBuffer -> ByteOffset -> Max a -> Effect (Maybe ByteLength)
putArrayBufferMax Proxy b o {end} =
  putArrayBuffer b o end

readArrayBufferMax :: forall a. DecodeArrayBuffer a => DynamicByteLength a => Proxy a -> ArrayBuffer -> ByteOffset -> Effect (Maybe (Max a))
readArrayBufferMax Proxy b o = map (\end -> {end}) <$> readArrayBuffer b o

byteLengthMax :: forall a. DynamicByteLength a => Proxy a -> Max a -> Effect ByteLength
byteLengthMax Proxy {end} = byteLength end

data DecidedMax
  = DecidedMaxNumber (Max Number)

makeDecidedMax :: DecidedValue -> DecidedMax
makeDecidedMax v = case v of
  DecidedValueNumber n -> DecidedMaxNumber { end: n }

derive instance genericDecidedMax :: Generic DecidedMax _

instance eqDecidedMax :: Eq DecidedMax where
  eq = genericEq

instance showDecidedMax :: Show DecidedMax where
  show = genericShow

instance encodeJsonDecidedMax :: EncodeJson DecidedMax where
  encodeJson x = case x of
    DecidedMaxNumber y -> "numberMax" := encodeJsonMax (Proxy :: Proxy Number) y ~> jsonEmptyObject

instance decodeJsonDecidedMax :: DecodeJson DecidedMax where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = do
        j <- o .: "numberMax"
        DecidedMaxNumber <$> decodeJsonMax (Proxy :: Proxy Number) j
    decodeNumber

instance encodeArrayBufferDecidedMax :: EncodeArrayBuffer DecidedMax where
  putArrayBuffer b o x = case x of
    DecidedMaxNumber {end} -> do
      mW <- putArrayBuffer b o (Uint8 (UInt.fromInt 0))
      case mW of
        Nothing -> pure Nothing
        Just w -> do
          mW' <- putArrayBufferMax (Proxy :: Proxy Float64BE) b (o + w) {end: Float64BE end}
          case mW' of
            Nothing -> pure (Just w)
            Just w' -> pure (Just (w + w'))

instance decodeArrayBufferDecidedMax :: DecodeArrayBuffer DecidedMax where
  readArrayBuffer b o = do
    mC <- readArrayBuffer b o
    case mC of
      Nothing -> pure Nothing
      Just (Uint8 c)
        | c == UInt.fromInt 0 ->
          map (\{end: Float64BE end} -> DecidedMaxNumber {end})
            <$> readArrayBufferMax (Proxy :: Proxy Float64BE) b (o + 1)
        | otherwise -> pure Nothing

instance dynamicByteLengthDecidedMax :: DynamicByteLength DecidedMax where
  byteLength x = case x of
    DecidedMaxNumber {end} ->
      (_ + 1) <$> byteLengthMax (Proxy :: Proxy Float64BE) {end: Float64BE end}

instance arbitraryDecidedMax :: Arbitrary DecidedMax where
  arbitrary = oneOf $ NonEmpty (DecidedMaxNumber <$> genMax (Proxy :: Proxy Number)) []
