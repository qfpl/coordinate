module Data.GPS.Coordinate.Coordinate(
  Coordinate
, coordinate
) where

import Prelude(Eq, Show)
import Control.Lens(Iso', iso)
import Data.GPS.Coordinate.Latitude
import Data.GPS.Coordinate.Longitude

data Coordinate =
  Coordinate
    Latitude
    Longitude
  deriving (Eq, Show)

coordinate ::
  Iso' (Latitude, Longitude) Coordinate
coordinate =
  iso (\(lat, lon) -> Coordinate lat lon) (\(Coordinate lat lon) -> (lat, lon))
