module Timeline.Time.Bounds where

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

type Bounds a
  = { begin :: a, end :: a }

genBounds :: forall a. Arbitrary a => Proxy a -> Gen (Bounds a)
genBounds Proxy = do
  begin <- arbitrary
  end <- arbitrary
  pure { begin, end }

encodeJsonBounds :: forall a. EncodeJson a => Proxy a -> Bounds a -> Json
encodeJsonBounds Proxy { begin, end } = "begin" := begin ~> "end" := end ~> jsonEmptyObject

decodeJsonBounds :: forall a. DecodeJson a => Proxy a -> Json -> Either String (Bounds a)
decodeJsonBounds Proxy json = do
  o <- decodeJson json
  begin <- o .: "begin"
  end <- o .: "end"
  pure { begin, end }

putArrayBufferBounds :: forall a. EncodeArrayBuffer a => Proxy a -> ArrayBuffer -> ByteOffset -> Bounds a -> Effect (Maybe ByteLength)
putArrayBufferBounds Proxy b o {begin,end} =
  putArrayBuffer b o (Tuple begin end)

readArrayBufferBounds :: forall a. DecodeArrayBuffer a => DynamicByteLength a => Proxy a -> ArrayBuffer -> ByteOffset -> Effect (Maybe (Bounds a))
readArrayBufferBounds Proxy b o = map (\(Tuple begin end) -> {begin,end}) <$> readArrayBuffer b o

byteLengthBounds :: forall a. DynamicByteLength a => Proxy a -> Bounds a -> Effect ByteLength
byteLengthBounds Proxy {begin,end} = byteLength (Tuple begin end)

data DecidedBounds
  = DecidedBoundsNumber (Bounds Number)

makeDecidedBounds :: { begin :: DecidedValue, end :: DecidedValue } -> Maybe DecidedBounds
makeDecidedBounds { begin, end } = case Tuple begin end of
  Tuple (DecidedValueNumber begin') (DecidedValueNumber end') -> Just (DecidedBoundsNumber { begin: begin', end: end' })
  _ -> Nothing

unmakeDecidedBounds :: DecidedBounds -> { begin :: DecidedValue, end :: DecidedValue }
unmakeDecidedBounds b = case b of
  DecidedBoundsNumber { begin, end } -> { begin: DecidedValueNumber begin, end: DecidedValueNumber end }

derive instance genericDecidedBounds :: Generic DecidedBounds _

instance eqDecidedBounds :: Eq DecidedBounds where
  eq = genericEq

instance ordDecidedBounds :: Ord DecidedBounds where
  compare = genericCompare

instance showDecidedBounds :: Show DecidedBounds where
  show = genericShow

instance encodeJsonDecidedBounds :: EncodeJson DecidedBounds where
  encodeJson x = case x of
    DecidedBoundsNumber y -> "numberBounds" := encodeJsonBounds (Proxy :: Proxy Number) y ~> jsonEmptyObject

instance decodeJsonDecidedBounds :: DecodeJson DecidedBounds where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = do
        j <- o .: "numberBounds"
        DecidedBoundsNumber <$> decodeJsonBounds (Proxy :: Proxy Number) j
    decodeNumber

instance encodeArrayBufferDecidedBounds :: EncodeArrayBuffer DecidedBounds where
  putArrayBuffer b o x = case x of
    DecidedBoundsNumber {begin,end} -> do
      mW <- putArrayBuffer b o (Uint8 (UInt.fromInt 0))
      case mW of
        Nothing -> pure Nothing
        Just w -> do
          mW' <- putArrayBufferBounds (Proxy :: Proxy Float64BE) b (o + w) {begin: Float64BE begin, end: Float64BE end}
          case mW' of
            Nothing -> pure (Just w)
            Just w' -> pure (Just (w + w'))

instance decodeArrayBufferDecidedBounds :: DecodeArrayBuffer DecidedBounds where
  readArrayBuffer b o = do
    mC <- readArrayBuffer b o
    case mC of
      Nothing -> pure Nothing
      Just (Uint8 c)
        | c == UInt.fromInt 0 ->
          map (\{begin: Float64BE begin, end: Float64BE end} -> DecidedBoundsNumber {begin,end})
            <$> readArrayBufferBounds (Proxy :: Proxy Float64BE) b (o + 1)
        | otherwise -> pure Nothing

instance dynamicByteLengthDecidedBounds :: DynamicByteLength DecidedBounds where
  byteLength x = case x of
    DecidedBoundsNumber {begin,end} ->
      (_ + 1) <$> byteLengthBounds (Proxy :: Proxy Float64BE) {begin: Float64BE begin, end: Float64BE end}

instance arbitraryDecidedBounds :: Arbitrary DecidedBounds where
  arbitrary = oneOf $ NonEmpty (DecidedBoundsNumber <$> genBounds (Proxy :: Proxy Number)) []
