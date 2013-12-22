module Data.GPS.Coordinate.Coordinate(
  Coordinate
, coordinate
, coordinate'
, coordinateLat
, coordinateLon
, coordinateLatlon
) where

import Prelude(Eq, Show, (.))
import Control.Lens(Iso', iso, mapping, swapped, withIso)
import Data.GPS.Coordinate.Latitude
import Data.GPS.Coordinate.Longitude
import Data.GPS.Coordinate.DegreesLatitude
import Data.GPS.Coordinate.DegreesLongitude
import Data.GPS.Coordinate.Minutes
import Data.GPS.Coordinate.Seconds

data Coordinate =
  Coordinate
    Latitude
    Longitude
  deriving (Eq, Show)

coordinate ::
  Iso' (Latitude, Longitude) Coordinate
coordinate =
  iso (\(lat, lon) -> Coordinate lat lon) (\(Coordinate lat lon) -> (lat, lon))

coordinate' ::
  Iso' (Longitude, Latitude) Coordinate
coordinate' =
  swapped . coordinate

coordinateLat ::
  Iso' ((DegreesLatitude, Minutes, Seconds), Longitude) Coordinate
coordinateLat =
  swapped . mapping latitude . coordinate'

coordinateLon ::
  Iso' (Latitude, (DegreesLongitude, Minutes, Seconds)) Coordinate
coordinateLon =
  mapping longitude . coordinate

coordinateLatlon ::
  Iso' ((DegreesLatitude, Minutes, Seconds), (DegreesLongitude, Minutes, Seconds)) Coordinate
coordinateLatlon =
  iso (\((td, tm, ts), (nd, nm, ns)) -> Coordinate (withIso latitude (\k _ -> k (td, tm, ts))) (withIso longitude (\k _ -> k (nd, nm, ns))))
      (\(Coordinate lat lon) -> (withIso latitude (\_ k -> k lat), withIso longitude (\_ k -> k lon)))
