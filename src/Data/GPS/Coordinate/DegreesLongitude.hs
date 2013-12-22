module Data.GPS.Coordinate.DegreesLongitude(
  DegreesLongitude
, degreesLongitude
) where

import Prelude(Int, Bool(..), Eq, Show, Ord(..), (&&))
import Data.Maybe(Maybe(..))
import Control.Lens(Prism', prism')

newtype DegreesLongitude =
  DegreesLongitude Int
  deriving (Eq, Show)

degreesLongitude ::
  Prism' Int DegreesLongitude
degreesLongitude =
  prism'
    (\(DegreesLongitude i) -> i)
    (\i -> case i >= -180 && i <= 180 of
             True -> Just (DegreesLongitude i)
             False -> Nothing)
