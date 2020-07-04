module Timeline.Time.Limit where

import Timeline.Time.Value (DecidedValue(..))
import Timeline.Time.Min
  ( Min
  , putArrayBufferMin
  , readArrayBufferMin
  , byteLengthMin
  )
import Timeline.Time.Max
  ( Max
  , putArrayBufferMax
  , readArrayBufferMax
  , byteLengthMax
  )
import Timeline.Time.Bounds
  ( Bounds
  , putArrayBufferBounds
  , readArrayBufferBounds
  , byteLengthBounds
  )
import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)
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
import Data.ArrayBuffer.Class.Types (Float64BE (..), Uint8 (..))
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)
import Type.Proxy (Proxy(..))

data Limit a
  = LimitBounds (Bounds a)
  | LimitMin (Min a)
  | LimitMax (Max a)

derive instance genericLimit :: Generic a a' => Generic (Limit a) _

instance functorLimit :: Functor Limit where
  map f x = case x of
    LimitBounds y -> LimitBounds y {begin = f y.begin, end = f y.end}
    LimitMin y -> LimitMin y {begin = f y.begin}
    LimitMax y -> LimitMax y {end = f y.end}

instance foldableLimit :: Foldable Limit where
  foldr f acc x = case x of
    LimitBounds y -> f y.begin (f y.end acc)
    LimitMin y -> f y.begin acc
    LimitMax y -> f y.end acc
  foldl f acc x = case x of
    LimitBounds y -> f (f acc y.begin) y.end
    LimitMin y -> f acc y.begin
    LimitMax y -> f acc y.end
  foldMap f x = case x of
    LimitBounds y -> f y.begin <> f y.end
    LimitMin y -> f y.begin
    LimitMax y -> f y.end

instance traversableLimit :: Traversable Limit where
  traverse f x = case x of
    LimitBounds y -> (\begin end -> LimitBounds {begin,end}) <$> f y.begin <*> f y.end
    LimitMin y -> (\begin -> LimitMin {begin}) <$> f y.begin
    LimitMax y -> (\end -> LimitMax {end}) <$> f y.end
  sequence x = case x of
    LimitBounds y -> (\begin end -> LimitBounds {begin,end}) <$> y.begin <*> y.end
    LimitMin y -> (\begin -> LimitMin {begin}) <$> y.begin
    LimitMax y -> (\end -> LimitMax {end}) <$> y.end

instance eqLimit :: Eq a => Eq (Limit a) where
  eq x y = case Tuple x y of
    Tuple (LimitBounds x') (LimitBounds y') -> x' == y'
    Tuple (LimitMin x') (LimitMin y') -> x' == y'
    Tuple (LimitMax x') (LimitMax y') -> x' == y'
    _ -> false

instance showLimit :: Show a => Show (Limit a) where
  show x = case x of
    LimitBounds y -> "(LimitBounds " <> show y <> ")"
    LimitMin y -> "(LimitMin " <> show y <> ")"
    LimitMax y -> "(LimitMax " <> show y <> ")"

instance encodeJsonLimit :: EncodeJson a => EncodeJson (Limit a) where
  encodeJson x = case x of
    LimitBounds y -> "limitBounds" := y ~> jsonEmptyObject
    LimitMin y -> "limitMin" := y ~> jsonEmptyObject
    LimitMax y -> "limitMax" := y ~> jsonEmptyObject

instance decodeJsonLimit :: DecodeJson a => DecodeJson (Limit a) where
  decodeJson json = do
    o <- decodeJson json
    let
      limitBounds = LimitBounds <$> o .: "limitBounds"

      limitMin = LimitMin <$> o .: "limitMin"

      limitMax = LimitMax <$> o .: "limitMax"
    limitBounds <|> limitMin <|> limitMax

instance encodeArrayBufferLimit :: EncodeArrayBuffer a => EncodeArrayBuffer (Limit a) where
  putArrayBuffer b o x = case x of
    LimitBounds y -> do
      mW <- putArrayBuffer b o (Uint8 (UInt.fromInt 0))
      case mW of
        Nothing -> pure Nothing
        Just w -> do
          mW' <- putArrayBufferBounds (Proxy :: Proxy a) b (o + w) y
          case mW' of
            Nothing -> pure (Just w)
            Just w' -> pure (Just (w + w'))
    LimitMin y -> do
      mW <- putArrayBuffer b o (Uint8 (UInt.fromInt 1))
      case mW of
        Nothing -> pure Nothing
        Just w -> do
          mW' <- putArrayBufferMin (Proxy :: Proxy a) b (o + w) y
          case mW' of
            Nothing -> pure (Just w)
            Just w' -> pure (Just (w + w'))
    LimitMax y -> do
      mW <- putArrayBuffer b o (Uint8 (UInt.fromInt 2))
      case mW of
        Nothing -> pure Nothing
        Just w -> do
          mW' <- putArrayBufferMax (Proxy :: Proxy a) b (o + w) y
          case mW' of
            Nothing -> pure (Just w)
            Just w' -> pure (Just (w + w'))

instance decodeArrayBufferLimit :: (DecodeArrayBuffer a, DynamicByteLength a) => DecodeArrayBuffer (Limit a) where
  readArrayBuffer b o = do
    mC <- readArrayBuffer b o
    case mC of
      Nothing -> pure Nothing
      Just (Uint8 c)
        | c == UInt.fromInt 0 -> map LimitBounds <$> readArrayBufferBounds (Proxy :: Proxy a) b (o + 1)
        | c == UInt.fromInt 1 -> map LimitMin <$> readArrayBufferMin (Proxy :: Proxy a) b (o + 1)
        | c == UInt.fromInt 2 -> map LimitMax <$> readArrayBufferMax (Proxy :: Proxy a) b (o + 1)
        | otherwise -> pure Nothing

instance dynamicByteLengthLimit :: DynamicByteLength a => DynamicByteLength (Limit a) where
  byteLength x = (_ + 1) <$> case x of
    LimitBounds y -> byteLengthBounds (Proxy :: Proxy a) y
    LimitMin y -> byteLengthMin (Proxy :: Proxy a) y
    LimitMax y -> byteLengthMax (Proxy :: Proxy a) y

instance arbitraryLimit :: Arbitrary a => Arbitrary (Limit a) where
  arbitrary = oneOf $ NonEmpty (LimitBounds <$> arbitrary) [ LimitMin <$> arbitrary, LimitMax <$> arbitrary ]

data DecidedLimit
  = DecidedLimitNumber (Limit Number)

makeDecidedLimit :: { begin :: Maybe DecidedValue, end :: Maybe DecidedValue } -> Maybe DecidedLimit
makeDecidedLimit { begin, end } = case Tuple begin end of
  Tuple (Just (DecidedValueNumber begin')) (Just (DecidedValueNumber end')) -> Just (DecidedLimitNumber (LimitBounds { begin: begin', end: end' }))
  Tuple (Just (DecidedValueNumber begin')) Nothing -> Just (DecidedLimitNumber (LimitMin { begin: begin' }))
  Tuple Nothing (Just (DecidedValueNumber end')) -> Just (DecidedLimitNumber (LimitMax { end: end' }))
  _ -> Nothing -- TODO other units

unmakeDecidedLimit :: DecidedLimit -> { begin :: Maybe DecidedValue, end :: Maybe DecidedValue }
unmakeDecidedLimit l = case l of
  DecidedLimitNumber l' -> case l' of
    LimitBounds { begin, end } -> { begin: Just (DecidedValueNumber begin), end: Just (DecidedValueNumber end) }
    LimitMin { begin } -> { begin: Just (DecidedValueNumber begin), end: Nothing }
    LimitMax { end } -> { begin: Nothing, end: Just (DecidedValueNumber end) }

derive instance genericDecidedLimit :: Generic DecidedLimit _

instance eqDecidedLimit :: Eq DecidedLimit where
  eq = genericEq

instance showDecidedLimit :: Show DecidedLimit where
  show = genericShow

instance encodeJsonDecidedLimit :: EncodeJson DecidedLimit where
  encodeJson x = case x of
    DecidedLimitNumber y -> "numberLimit" := y ~> jsonEmptyObject

instance decodeJsonDecidedLimit :: DecodeJson DecidedLimit where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = DecidedLimitNumber <$> o .: "numberLimit"
    decodeNumber

instance encodeArrayBufferDecidedLimit :: EncodeArrayBuffer DecidedLimit where
  putArrayBuffer b o x = case x of
    DecidedLimitNumber y -> do
      mW <- putArrayBuffer b o (Uint8 (UInt.fromInt 0))
      case mW of
        Nothing -> pure Nothing
        Just w -> do
          mW' <- putArrayBuffer b (o + w) (map Float64BE y)
          case mW' of
            Nothing -> pure Nothing
            Just w' -> pure (Just (w + w'))

instance decodeArrayBufferDecidedLimit :: DecodeArrayBuffer DecidedLimit where
  readArrayBuffer b o = do
    mC <- readArrayBuffer b o
    case mC of
      Nothing -> pure Nothing
      Just (Uint8 c)
        | c == UInt.fromInt 0 -> map (DecidedLimitNumber <<< map (\(Float64BE x) -> x)) <$> readArrayBuffer b (o + 1)
        | otherwise -> pure Nothing

instance dynamicByteLengthDecidedLimit :: DynamicByteLength DecidedLimit where
  byteLength x = (_ + 1) <$> case x of
    DecidedLimitNumber y -> byteLength (map Float64BE y)

instance arbitraryDecidedLimit :: Arbitrary DecidedLimit where
  arbitrary = oneOf $ NonEmpty (DecidedLimitNumber <$> arbitrary) []
