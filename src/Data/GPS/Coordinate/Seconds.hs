module Data.GPS.Coordinate.Seconds(
  Seconds
, HasSeconds(..)
, nSeconds
) where

import Prelude(Double, Bool(..), Eq, Show, Ord(..), id, (&&))
import Data.Maybe(Maybe(..))
import Control.Lens(Prism', Lens', prism')

newtype Seconds =
  Seconds Double
  deriving (Eq, Show)

nSeconds ::
  Prism' Double Seconds
nSeconds =
  prism'
    (\(Seconds d) -> d)
    (\d -> case d >= 0 && d < 60 of
             True -> Just (Seconds d)
             False -> Nothing)

class HasSeconds t where
  seconds ::
    Lens' t Seconds

instance HasSeconds Seconds where
  seconds =
    id
