module Data.GPS.Coordinate.DegreesLongitude(
  DegreesLongitude
, HasDegreesLongitude(..)
, nDegreesLongitude
) where

import Prelude(Int, Bool(..), Eq, Show, Ord(..), id, (&&))
import Data.Maybe(Maybe(..))
import Control.Lens(Prism', Lens', prism')

newtype DegreesLongitude =
  DegreesLongitude Int
  deriving (Eq, Show)

nDegreesLongitude ::
  Prism' Int DegreesLongitude
nDegreesLongitude  =
  prism'
    (\(DegreesLongitude i) -> i)
    (\i -> case i >= -180 && i <= 180 of
             True -> Just (DegreesLongitude i)
             False -> Nothing)

class HasDegreesLongitude t where
  degreesLongitude ::
    Lens' t DegreesLongitude

instance HasDegreesLongitude DegreesLongitude where
  degreesLongitude =
    id
