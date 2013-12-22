module Data.GPS.Coordinate.DegreesLatitude(
  DegreesLatitude
, degreesLatitude
) where

import Prelude(Int, Bool(..), Eq, Show, Ord(..), (&&))
import Data.Maybe(Maybe(..))
import Control.Lens(Prism', prism')

newtype DegreesLatitude =
  DegreesLatitude Int
  deriving (Eq, Show)

degreesLatitude ::
  Prism' Int DegreesLatitude
degreesLatitude =
  prism'
    (\(DegreesLatitude i) -> i)
    (\i -> case i >= -90 && i <= 90 of
             True -> Just (DegreesLatitude i)
             False -> Nothing)
