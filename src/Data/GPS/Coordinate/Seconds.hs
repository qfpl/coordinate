module Data.GPS.Coordinate.Seconds(
  Seconds
, seconds
) where

import Prelude(Double, Bool(..), Eq, Show, Ord(..), (&&))
import Data.Maybe(Maybe(..))
import Control.Lens(Prism', prism')

newtype Seconds =
  Seconds Double
  deriving (Eq, Show)

seconds ::
  Prism' Double Seconds
seconds =
  prism'
    (\(Seconds d) -> d)
    (\d -> case d >= 0 && d < 60 of
             True -> Just (Seconds d)
             False -> Nothing)
