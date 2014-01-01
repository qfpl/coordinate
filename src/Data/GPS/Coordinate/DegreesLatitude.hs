module Data.GPS.Coordinate.DegreesLatitude(
  DegreesLatitude
, HasDegreesLatitude(..)
, nDegreesLatitude
) where

import Prelude(Int, Bool(..), Eq, Show, Ord(..), id, (&&))
import Data.Maybe(Maybe(..))
import Control.Lens(Prism', Lens', prism')

newtype DegreesLatitude =
  DegreesLatitude Int
  deriving (Eq, Ord, Show)

nDegreesLatitude ::
  Prism' Int DegreesLatitude
nDegreesLatitude =
  prism'
    (\(DegreesLatitude i) -> i)
    (\i -> case i >= -90 && i <= 90 of
             True -> Just (DegreesLatitude i)
             False -> Nothing)

class HasDegreesLatitude t where
  degreesLatitude ::
    Lens' t DegreesLatitude

instance HasDegreesLatitude DegreesLatitude where
  degreesLatitude =
    id

