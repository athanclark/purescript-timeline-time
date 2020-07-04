module Timeline.Time.MaybeLimit where

import Timeline.Time.Unit (DecidedUnit(..))
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
  , encodeJson
  , decodeJson
  , (~>)
  , jsonEmptyObject
  , (:=)
  , (.:)
  , fail
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

data MaybeLimit a
  = JustLimitBounds (Bounds a)
  | JustLimitMin (Min a)
  | JustLimitMax (Max a)
  | NothingLimit

derive instance genericMaybeLimit :: Generic a a' => Generic (MaybeLimit a) _

instance functorMaybeLimit :: Functor MaybeLimit where
  map f x = case x of
    JustLimitBounds y -> JustLimitBounds y {begin = f y.begin, end = f y.end}
    JustLimitMin y -> JustLimitMin y {begin = f y.begin}
    JustLimitMax y -> JustLimitMax y {end = f y.end}
    NothingLimit -> NothingLimit

instance foldableMaybeLimit :: Foldable MaybeLimit where
  foldr f acc x = case x of
    JustLimitBounds y -> f y.begin (f y.end acc)
    JustLimitMin y -> f y.begin acc
    JustLimitMax y -> f y.end acc
    NothingLimit -> acc
  foldl f acc x = case x of
    JustLimitBounds y -> f (f acc y.begin) y.end
    JustLimitMin y -> f acc y.begin
    JustLimitMax y -> f acc y.end
    NothingLimit -> acc
  foldMap f x = case x of
    JustLimitBounds y -> f y.begin <> f y.end
    JustLimitMin y -> f y.begin
    JustLimitMax y -> f y.end
    NothingLimit -> mempty

instance traversableMaybeLimit :: Traversable MaybeLimit where
  traverse f x = case x of
    JustLimitBounds y -> (\begin end -> JustLimitBounds {begin,end}) <$> f y.begin <*> f y.end
    JustLimitMin y -> (\begin -> JustLimitMin {begin}) <$> f y.begin
    JustLimitMax y -> (\end -> JustLimitMax {end}) <$> f y.end
    NothingLimit -> pure NothingLimit
  sequence x = case x of
    JustLimitBounds y -> (\begin end -> JustLimitBounds {begin,end}) <$> y.begin <*> y.end
    JustLimitMin y -> (\begin -> JustLimitMin {begin}) <$> y.begin
    JustLimitMax y -> (\end -> JustLimitMax {end}) <$> y.end
    NothingLimit -> pure NothingLimit

instance eqMaybeLimit :: Eq a => Eq (MaybeLimit a) where
  eq x y = case Tuple x y of
    Tuple NothingLimit NothingLimit -> true
    Tuple (JustLimitBounds x') (JustLimitBounds y') -> x' == y'
    Tuple (JustLimitMin x') (JustLimitMin y') -> x' == y'
    Tuple (JustLimitMax x') (JustLimitMax y') -> x' == y'
    _ -> false

instance ordMaybeLimit :: Ord a => Ord (MaybeLimit a) where
  compare x y = case Tuple x y of
    Tuple NothingLimit NothingLimit -> EQ
    Tuple (JustLimitBounds x') (JustLimitBounds y') -> compare x' y'
    Tuple (JustLimitMin x') (JustLimitMin y') -> compare x' y'
    Tuple (JustLimitMax x') (JustLimitMax y') -> compare x' y'
    Tuple (JustLimitBounds _) _ -> LT
    Tuple (JustLimitMin _) (JustLimitBounds _) -> GT
    Tuple (JustLimitMin _) _ -> LT
    Tuple (JustLimitMax _) (JustLimitBounds _) -> GT
    Tuple (JustLimitMax _) (JustLimitMin _) -> GT
    Tuple (JustLimitMax _) _ -> LT
    Tuple NothingLimit _ -> GT

instance showMaybeLimit :: Show a => Show (MaybeLimit a) where
  show x = case x of
    JustLimitBounds y -> "(JustLimitBounds " <> show y <> ")"
    JustLimitMin y -> "(JustLimitMin " <> show y <> ")"
    JustLimitMax y -> "(JustLimitMax " <> show y <> ")"
    NothingLimit -> "NothingLimit"

instance encodeJsonMaybeLimit :: EncodeJson a => EncodeJson (MaybeLimit a) where
  encodeJson x = case x of
    JustLimitBounds y -> "justLimitBounds" := y ~> jsonEmptyObject
    JustLimitMin y -> "justLimitMin" := y ~> jsonEmptyObject
    JustLimitMax y -> "justLimitMax" := y ~> jsonEmptyObject
    NothingLimit -> encodeJson "nothingLimit"

instance decodeJsonMaybeLimit :: DecodeJson a => DecodeJson (MaybeLimit a) where
  decodeJson json = do
    let
      obj = do
        o <- decodeJson json
        let
          limitBounds = JustLimitBounds <$> o .: "justLimitBounds"

          limitMin = JustLimitMin <$> o .: "justLimitMin"

          limitMax = JustLimitMax <$> o .: "justLimitMax"
        limitBounds <|> limitMin <|> limitMax

      str = do
        s <- decodeJson json
        case s of
          "nothingLimit" -> pure NothingLimit
          _ -> fail $ "Unrecognized MaybeLimit: " <> s
    obj <|> str

instance encodeArrayBufferMaybeLimit :: EncodeArrayBuffer a => EncodeArrayBuffer (MaybeLimit a) where
  putArrayBuffer b o x = case x of
    JustLimitBounds y -> do
      mW <- putArrayBuffer b o (Uint8 (UInt.fromInt 0))
      case mW of
        Nothing -> pure Nothing
        Just w -> do
          mW' <- putArrayBufferBounds (Proxy :: Proxy a) b (o + w) y
          case mW' of
            Nothing -> pure (Just w)
            Just w' -> pure (Just (w + w'))
    JustLimitMin y -> do
      mW <- putArrayBuffer b o (Uint8 (UInt.fromInt 1))
      case mW of
        Nothing -> pure Nothing
        Just w -> do
          mW' <- putArrayBufferMin (Proxy :: Proxy a) b (o + w) y
          case mW' of
            Nothing -> pure (Just w)
            Just w' -> pure (Just (w + w'))
    JustLimitMax y -> do
      mW <- putArrayBuffer b o (Uint8 (UInt.fromInt 2))
      case mW of
        Nothing -> pure Nothing
        Just w -> do
          mW' <- putArrayBufferMax (Proxy :: Proxy a) b (o + w) y
          case mW' of
            Nothing -> pure (Just w)
            Just w' -> pure (Just (w + w'))
    NothingLimit -> putArrayBuffer b o (Uint8 (UInt.fromInt 3))

instance decodeArrayBufferMaybeLimit :: (DecodeArrayBuffer a, DynamicByteLength a) => DecodeArrayBuffer (MaybeLimit a) where
  readArrayBuffer b o = do
    mC <- readArrayBuffer b o
    case mC of
      Nothing -> pure Nothing
      Just (Uint8 c)
        | c == UInt.fromInt 0 -> map JustLimitBounds <$> readArrayBufferBounds (Proxy :: Proxy a) b (o + 1)
        | c == UInt.fromInt 1 -> map JustLimitMin <$> readArrayBufferMin (Proxy :: Proxy a) b (o + 1)
        | c == UInt.fromInt 2 -> map JustLimitMax <$> readArrayBufferMax (Proxy :: Proxy a) b (o + 1)
        | c == UInt.fromInt 3 -> pure (Just NothingLimit)
        | otherwise -> pure Nothing

instance dynamicByteLengthMaybeLimit :: DynamicByteLength a => DynamicByteLength (MaybeLimit a) where
  byteLength x = (_ + 1) <$> case x of
    JustLimitBounds y -> byteLengthBounds (Proxy :: Proxy a) y
    JustLimitMin y -> byteLengthMin (Proxy :: Proxy a) y
    JustLimitMax y -> byteLengthMax (Proxy :: Proxy a) y
    NothingLimit -> pure 0

instance arbitraryMaybeLimit :: Arbitrary a => Arbitrary (MaybeLimit a) where
  arbitrary = oneOf $ NonEmpty (JustLimitBounds <$> arbitrary) [ JustLimitMin <$> arbitrary, JustLimitMax <$> arbitrary, pure NothingLimit ]

data DecidedMaybeLimit
  = DecidedMaybeLimitNumber (MaybeLimit Number)

makeDecidedMaybeLimit :: DecidedUnit -> { begin :: Maybe DecidedValue, end :: Maybe DecidedValue } -> Maybe DecidedMaybeLimit
makeDecidedMaybeLimit unit { begin, end } = case {unit,begin,end} of
  { unit: DecidedUnitNumber
    , begin: Just (DecidedValueNumber begin')
    , end: Just (DecidedValueNumber end')
    } -> Just $ DecidedMaybeLimitNumber $ JustLimitBounds { begin: begin', end: end' }
  { unit: DecidedUnitNumber
    , begin: Just (DecidedValueNumber begin')
    , end: Nothing
    } -> Just $ DecidedMaybeLimitNumber $ JustLimitMin { begin: begin' }
  { unit: DecidedUnitNumber
    , begin: Nothing
    , end: Just (DecidedValueNumber end')
    } -> Just $ DecidedMaybeLimitNumber $ JustLimitMax { end: end' }
  { unit: DecidedUnitNumber
    , begin: Nothing
    , end: Nothing
    } -> Just $ DecidedMaybeLimitNumber NothingLimit
  _ -> Nothing

unmakeDecidedMaybeLimit :: DecidedMaybeLimit -> { begin :: Maybe DecidedValue, end :: Maybe DecidedValue, unit :: DecidedUnit }
unmakeDecidedMaybeLimit l = case l of
  DecidedMaybeLimitNumber limit -> case limit of
    JustLimitBounds { begin, end } -> { begin: Just (DecidedValueNumber begin), end: Just (DecidedValueNumber end), unit: DecidedUnitNumber }
    JustLimitMin { begin } -> { begin: Just (DecidedValueNumber begin), end: Nothing, unit: DecidedUnitNumber }
    JustLimitMax { end } -> { begin: Nothing, end: Just (DecidedValueNumber end), unit: DecidedUnitNumber }
    NothingLimit -> { begin: Nothing, end: Nothing, unit: DecidedUnitNumber }

makeDecidedMaybeLimitViaMaybeLimit :: DecidedUnit -> MaybeLimit DecidedValue -> Maybe DecidedMaybeLimit
makeDecidedMaybeLimitViaMaybeLimit unit mL = case mL of
  JustLimitBounds {begin,end} -> makeDecidedMaybeLimit unit {begin: Just begin, end: Just end}
  JustLimitMin {begin} -> makeDecidedMaybeLimit unit {begin: Just begin, end: Nothing}
  JustLimitMax {end} -> makeDecidedMaybeLimit unit {begin: Nothing, end: Just end}
  NothingLimit -> makeDecidedMaybeLimit unit {begin: Nothing, end: Nothing}

unmakeDecidedMaybeLimitWithMaybeLimit :: DecidedMaybeLimit -> { limit :: MaybeLimit DecidedValue, unit :: DecidedUnit }
unmakeDecidedMaybeLimitWithMaybeLimit l = case l of
  DecidedMaybeLimitNumber limit -> case limit of
    JustLimitBounds { begin, end } -> { limit: JustLimitBounds {begin: DecidedValueNumber begin, end: DecidedValueNumber end}, unit: DecidedUnitNumber }
    JustLimitMin { begin } -> { limit: JustLimitMin {begin: DecidedValueNumber begin}, unit: DecidedUnitNumber }
    JustLimitMax { end } -> { limit: JustLimitMax {end: DecidedValueNumber end}, unit: DecidedUnitNumber }
    NothingLimit -> { limit: NothingLimit, unit: DecidedUnitNumber }

getMaybeLimitDecidedUnit :: DecidedMaybeLimit -> DecidedUnit
getMaybeLimitDecidedUnit l = case l of
  DecidedMaybeLimitNumber _ -> DecidedUnitNumber

derive instance genericDecidedMaybeLimit :: Generic DecidedMaybeLimit _

instance eqDecidedMaybeLimit :: Eq DecidedMaybeLimit where
  eq = genericEq

instance showDecidedMaybeLimit :: Show DecidedMaybeLimit where
  show = genericShow

instance encodeJsonDecidedMaybeLimit :: EncodeJson DecidedMaybeLimit where
  encodeJson x = case x of
    DecidedMaybeLimitNumber y -> "numberMaybeLimit" := y ~> jsonEmptyObject

instance decodeJsonDecidedMaybeLimit :: DecodeJson DecidedMaybeLimit where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = DecidedMaybeLimitNumber <$> o .: "numberMaybeLimit"
    decodeNumber

instance encodeArrayBufferDecidedMaybeLimit :: EncodeArrayBuffer DecidedMaybeLimit where
  putArrayBuffer b o x = case x of
    DecidedMaybeLimitNumber y -> do
      mW <- putArrayBuffer b o (Uint8 (UInt.fromInt 0))
      case mW of
        Nothing -> pure Nothing
        Just w -> do
          mW' <- putArrayBuffer b (o + w) (map Float64BE y)
          case mW' of
            Nothing -> pure Nothing
            Just w' -> pure (Just (w + w'))

instance decodeArrayBufferDecidedMaybeLimit :: DecodeArrayBuffer DecidedMaybeLimit where
  readArrayBuffer b o = do
    mC <- readArrayBuffer b o
    case mC of
      Nothing -> pure Nothing
      Just (Uint8 c)
        | c == UInt.fromInt 0 -> map (DecidedMaybeLimitNumber <<< map (\(Float64BE x) -> x)) <$> readArrayBuffer b (o + 1)
        | otherwise -> pure Nothing

instance dynamicByteLengthDecidedMaybeLimit :: DynamicByteLength DecidedMaybeLimit where
  byteLength x = (_ + 1) <$> case x of
    DecidedMaybeLimitNumber y -> byteLength (map Float64BE y)

instance arbitraryDecidedMaybeLimit :: Arbitrary DecidedMaybeLimit where
  arbitrary = oneOf $ NonEmpty (DecidedMaybeLimitNumber <$> arbitrary) []
