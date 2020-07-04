module Timeline.Time.Span where

import Timeline.Time.Value (DecidedValue(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either)
import Data.UInt (fromInt) as UInt
import Data.NonEmpty (NonEmpty(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
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
import Data.ArrayBuffer.Class.Types (Float64BE (..), Uint8 (..))
import Effect (Effect)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, oneOf)
import Type.Proxy (Proxy(..))

type Span a
  = { start :: a, stop :: a }

genSpan :: forall a. Arbitrary a => Proxy a -> Gen (Span a)
genSpan Proxy = do
  start <- arbitrary
  stop <- arbitrary
  pure { start, stop }

encodeJsonSpan :: forall a. EncodeJson a => Proxy a -> Span a -> Json
encodeJsonSpan Proxy { start, stop } = "start" := start ~> "stop" := stop ~> jsonEmptyObject

decodeJsonSpan :: forall a. DecodeJson a => Proxy a -> Json -> Either String (Span a)
decodeJsonSpan Proxy json = do
  o <- decodeJson json
  start <- o .: "start"
  stop <- o .: "stop"
  pure { start, stop }

putArrayBufferSpan :: forall a. EncodeArrayBuffer a => Proxy a -> ArrayBuffer -> ByteOffset -> Span a -> Effect (Maybe ByteLength)
putArrayBufferSpan Proxy b o {start,stop} =
  putArrayBuffer b o (Tuple start stop)

readArrayBufferSpan :: forall a. DecodeArrayBuffer a => DynamicByteLength a => Proxy a -> ArrayBuffer -> ByteOffset -> Effect (Maybe (Span a))
readArrayBufferSpan Proxy b o = map (\(Tuple start stop) -> {start,stop}) <$> readArrayBuffer b o

byteLengthSpan :: forall a. DynamicByteLength a => Proxy a -> Span a -> Effect ByteLength
byteLengthSpan Proxy {start,stop} = byteLength (Tuple start stop)

data DecidedSpan
  = DecidedSpanNumber (Span Number)

makeDecidedSpan :: Span DecidedValue -> Maybe DecidedSpan
makeDecidedSpan { start, stop } = case Tuple start stop of
  Tuple (DecidedValueNumber start') (DecidedValueNumber stop') -> Just (DecidedSpanNumber { start: start', stop: stop' })
  _ -> Nothing

unmakeDecidedSpan :: DecidedSpan -> Span DecidedValue
unmakeDecidedSpan b = case b of
  DecidedSpanNumber { start, stop } -> { start: DecidedValueNumber start, stop: DecidedValueNumber stop }

derive instance genericDecidedSpan :: Generic DecidedSpan _

instance eqDecidedSpan :: Eq DecidedSpan where
  eq = genericEq

instance ordDecidedSpan :: Ord DecidedSpan where
  compare = genericCompare

instance showDecidedSpan :: Show DecidedSpan where
  show = genericShow

instance encodeJsonDecidedSpan :: EncodeJson DecidedSpan where
  encodeJson x = case x of
    DecidedSpanNumber y -> "numberSpan" := encodeJsonSpan (Proxy :: Proxy Number) y ~> jsonEmptyObject

instance decodeJsonDecidedSpan :: DecodeJson DecidedSpan where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = do
        j <- o .: "numberSpan"
        DecidedSpanNumber <$> decodeJsonSpan (Proxy :: Proxy Number) j
    decodeNumber

instance encodeArrayBufferDecidedSpan :: EncodeArrayBuffer DecidedSpan where
  putArrayBuffer b o x = case x of
    DecidedSpanNumber {start,stop} -> do
      mW <- putArrayBuffer b o (Uint8 (UInt.fromInt 0))
      case mW of
        Nothing -> pure Nothing
        Just w -> do
          mW' <- putArrayBufferSpan (Proxy :: Proxy Float64BE) b (o + w) {start: Float64BE start, stop: Float64BE stop}
          case mW' of
            Nothing -> pure (Just w)
            Just w' -> pure (Just (w + w'))

instance decodeArrayBufferDecidedSpan :: DecodeArrayBuffer DecidedSpan where
  readArrayBuffer b o = do
    mC <- readArrayBuffer b o
    case mC of
      Nothing -> pure Nothing
      Just (Uint8 c)
        | c == UInt.fromInt 0 ->
          map (\{start: Float64BE start, stop: Float64BE stop} -> DecidedSpanNumber {start,stop})
            <$> readArrayBufferSpan (Proxy :: Proxy Float64BE) b (o + 1)
        | otherwise -> pure Nothing

instance dynamicByteLengthDecidedSpan :: DynamicByteLength DecidedSpan where
  byteLength x = case x of
    DecidedSpanNumber {start,stop} ->
      (_ + 1) <$> byteLengthSpan (Proxy :: Proxy Float64BE) {start: Float64BE start, stop: Float64BE stop}


instance arbitraryDecidedSpan :: Arbitrary DecidedSpan where
  arbitrary = oneOf $ NonEmpty (DecidedSpanNumber <$> genSpan (Proxy :: Proxy Number)) []
