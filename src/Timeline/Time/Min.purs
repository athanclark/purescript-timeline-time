module Timeline.Time.Min where

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

type Min a
  = { begin :: a }

genMin :: forall a. Arbitrary a => Proxy a -> Gen (Min a)
genMin Proxy = do
  begin <- arbitrary
  pure { begin }

encodeJsonMin :: forall a. EncodeJson a => Proxy a -> Min a -> Json
encodeJsonMin Proxy { begin } = "begin" := begin ~> jsonEmptyObject

decodeJsonMin :: forall a. DecodeJson a => Proxy a -> Json -> Either JsonDecodeError (Min a)
decodeJsonMin Proxy json = do
  o <- decodeJson json
  begin <- o .: "begin"
  pure { begin }

putArrayBufferMin :: forall a. EncodeArrayBuffer a => Proxy a -> ArrayBuffer -> ByteOffset -> Min a -> Effect (Maybe ByteLength)
putArrayBufferMin Proxy b o {begin} =
  putArrayBuffer b o begin

readArrayBufferMin :: forall a. DecodeArrayBuffer a => DynamicByteLength a => Proxy a -> ArrayBuffer -> ByteOffset -> Effect (Maybe (Min a))
readArrayBufferMin Proxy b o = map (\begin -> {begin}) <$> readArrayBuffer b o

byteLengthMin :: forall a. DynamicByteLength a => Proxy a -> Min a -> Effect ByteLength
byteLengthMin Proxy {begin} = byteLength begin

data DecidedMin
  = DecidedMinNumber (Min Number)

makeDecidedMin :: DecidedValue -> DecidedMin
makeDecidedMin v = case v of
  DecidedValueNumber n -> DecidedMinNumber { begin: n }

derive instance genericDecidedMin :: Generic DecidedMin _

instance eqDecidedMin :: Eq DecidedMin where
  eq = genericEq

instance showDecidedMin :: Show DecidedMin where
  show = genericShow

instance encodeJsonDecidedMin :: EncodeJson DecidedMin where
  encodeJson x = case x of
    DecidedMinNumber y -> "numberMin" := encodeJsonMin (Proxy :: Proxy Number) y ~> jsonEmptyObject

instance decodeJsonDecidedMin :: DecodeJson DecidedMin where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = do
        j <- o .: "numberMin"
        DecidedMinNumber <$> decodeJsonMin (Proxy :: Proxy Number) j
    decodeNumber

instance encodeArrayBufferDecidedMin :: EncodeArrayBuffer DecidedMin where
  putArrayBuffer b o x = case x of
    DecidedMinNumber {begin} -> do
      mW <- putArrayBuffer b o (Uint8 (UInt.fromInt 0))
      case mW of
        Nothing -> pure Nothing
        Just w -> do
          mW' <- putArrayBufferMin (Proxy :: Proxy Float64BE) b (o + w) {begin: Float64BE begin}
          case mW' of
            Nothing -> pure (Just w)
            Just w' -> pure (Just (w + w'))

instance decodeArrayBufferDecidedMin :: DecodeArrayBuffer DecidedMin where
  readArrayBuffer b o = do
    mC <- readArrayBuffer b o
    case mC of
      Nothing -> pure Nothing
      Just (Uint8 c)
        | c == UInt.fromInt 0 ->
          map (\{begin: Float64BE begin} -> DecidedMinNumber {begin})
            <$> readArrayBufferMin (Proxy :: Proxy Float64BE) b (o + 1)
        | otherwise -> pure Nothing

instance dynamicByteLengthDecidedMin :: DynamicByteLength DecidedMin where
  byteLength x = case x of
    DecidedMinNumber {begin} ->
      (_ + 1) <$> byteLengthMin (Proxy :: Proxy Float64BE) {begin: Float64BE begin}

instance arbitraryDecidedMin :: Arbitrary DecidedMin where
  arbitrary = oneOf $ NonEmpty (DecidedMinNumber <$> genMin (Proxy :: Proxy Number)) []
