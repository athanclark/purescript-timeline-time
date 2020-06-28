module Timeline.Time.Class where

import Timeline.Time.Unit (DecidedUnit (..))
import Timeline.Time.Value (DecidedValue(..))
import Timeline.Time.Min (DecidedMin(..))
import Timeline.Time.Max (DecidedMax(..))
import Timeline.Time.Span (DecidedSpan(..))
import Timeline.Time.Bounds (DecidedBounds(..))
import Timeline.Time.Limit (DecidedLimit(..), Limit(..))
import Timeline.Time.MaybeLimit (DecidedMaybeLimit(..), MaybeLimit(..))
import Prelude
import Data.Tuple (Tuple (..))

-- | A class for formatting index types as strings, to be listed
class AsSecondaryString a where
  asSecondaryString :: a -> String

instance asSecondaryStringDecidedValue :: AsSecondaryString DecidedValue where
  asSecondaryString v = case v of
    DecidedValueNumber value -> "value: " <> show value

instance asSecondaryStringDecidedMin :: AsSecondaryString DecidedMin where
  asSecondaryString m = case m of
    DecidedMinNumber { begin } -> "beginning: " <> show begin

instance asSecondaryStringDecidedMax :: AsSecondaryString DecidedMax where
  asSecondaryString m = case m of
    DecidedMaxNumber { end } -> "end: " <> show end

instance asSecondaryStringDecidedSpan :: AsSecondaryString DecidedSpan where
  asSecondaryString s = case s of
    DecidedSpanNumber { start, stop } -> "start: " <> show start <> ", stop: " <> show stop

instance asSecondaryStringDecidedBounds :: AsSecondaryString DecidedBounds where
  asSecondaryString s = case s of
    DecidedBoundsNumber { begin, end } -> "beginning: " <> show begin <> ", end: " <> show end

instance asSecondaryStringDecidedLimit :: AsSecondaryString DecidedLimit where
  asSecondaryString l = case l of
    DecidedLimitNumber l' -> case l' of
      LimitBounds { begin, end } -> "beginning: " <> show begin <> ", end: " <> show end
      LimitMin { begin } -> "beginning: " <> show begin
      LimitMax { end } -> "end: " <> show end

instance asSecondaryStringDecidedMaybeLimit :: AsSecondaryString DecidedMaybeLimit where
  asSecondaryString l = case l of
    DecidedMaybeLimitNumber l' -> case l' of
      JustLimitBounds { begin, end } -> "beginning: " <> show begin <> ", end: " <> show end
      JustLimitMin { begin } -> "beginning: " <> show begin
      JustLimitMax { end } -> "end: " <> show end
      NothingLimit -> "no bounds"


class GetDecidedUnit a where
  getDecidedUnit :: a -> DecidedUnit

instance getDecidedUnitDecidedValue :: GetDecidedUnit DecidedValue where
  getDecidedUnit x = case x of
    DecidedValueNumber _ -> DecidedUnitNumber

instance getDecidedUnitDecidedMin :: GetDecidedUnit DecidedMin where
  getDecidedUnit x = case x of
    DecidedMinNumber _ -> DecidedUnitNumber

instance getDecidedUnitDecidedMax :: GetDecidedUnit DecidedMax where
  getDecidedUnit x = case x of
    DecidedMaxNumber _ -> DecidedUnitNumber

instance getDecidedUnitDecidedSpan :: GetDecidedUnit DecidedSpan where
  getDecidedUnit x = case x of
    DecidedSpanNumber _ -> DecidedUnitNumber

instance getDecidedUnitDecidedBounds :: GetDecidedUnit DecidedBounds where
  getDecidedUnit x = case x of
    DecidedBoundsNumber _ -> DecidedUnitNumber

instance getDecidedUnitDecidedLimit :: GetDecidedUnit DecidedLimit where
  getDecidedUnit x = case x of
    DecidedLimitNumber _ -> DecidedUnitNumber

instance getDecidedUnitDecidedMaybeLimit :: GetDecidedUnit DecidedMaybeLimit where
  getDecidedUnit x = case x of
    DecidedMaybeLimitNumber _ -> DecidedUnitNumber


data ConvertedTime a
  = Lossless a
  | Lossy String a
  | NoChange a

instance eqConvertedTime :: Eq a => Eq (ConvertedTime a) where
  eq x y = case Tuple x y of
    Tuple (Lossless x') (Lossless y') -> x' == y'
    Tuple (Lossy m x') (Lossy n y') -> m == n && x' == y'
    Tuple (NoChange x') (NoChange y') -> x' == y'
    _ -> false

instance showConvertedTime :: Show a => Show (ConvertedTime a) where
  show x = case x of
    Lossless y -> "(Lossless " <> show y <> ")"
    Lossy m y -> "(Lossy " <> show m <> " " <> show y <> ")"
    NoChange y -> "(NoChange " <> show y <> ")"

class ConvertTime a where
  convertTime :: DecidedUnit -> a -> ConvertedTime a

instance convertTimeDecidedValue :: ConvertTime DecidedValue where
  convertTime u x = case Tuple u x of
    Tuple DecidedUnitNumber (DecidedValueNumber _) -> NoChange x
    _ -> NoChange x -- FIXME adjust for DecidedUnitFoo

instance convertTimeDecidedMin :: ConvertTime DecidedMin where
  convertTime u x = case Tuple u x of
    Tuple DecidedUnitNumber (DecidedMinNumber _) -> NoChange x
    _ -> NoChange x -- FIXME adjust for DecidedUnitFoo

instance convertTimeDecidedMax :: ConvertTime DecidedMax where
  convertTime u x = case Tuple u x of
    Tuple DecidedUnitNumber (DecidedMaxNumber _) -> NoChange x
    _ -> NoChange x -- FIXME adjust for DecidedUnitFoo

instance convertTimeDecidedSpan :: ConvertTime DecidedSpan where
  convertTime u x = case Tuple u x of
    Tuple DecidedUnitNumber (DecidedSpanNumber _) -> NoChange x
    _ -> NoChange x -- FIXME adjust for DecidedUnitFoo

instance convertTimeDecidedBounds :: ConvertTime DecidedBounds where
  convertTime u x = case Tuple u x of
    Tuple DecidedUnitNumber (DecidedBoundsNumber _) -> NoChange x
    _ -> NoChange x -- FIXME adjust for DecidedUnitFoo

instance convertTimeDecidedLimit :: ConvertTime DecidedLimit where
  convertTime u x = case Tuple u x of
    Tuple DecidedUnitNumber (DecidedLimitNumber _) -> NoChange x
    _ -> NoChange x -- FIXME adjust for DecidedUnitFoo

instance convertTimeDecidedMaybeLimit :: ConvertTime DecidedMaybeLimit where
  convertTime u x = case Tuple u x of
    Tuple DecidedUnitNumber (DecidedMaybeLimitNumber _) -> NoChange x
    _ -> NoChange x -- FIXME adjust for DecidedUnitFoo
