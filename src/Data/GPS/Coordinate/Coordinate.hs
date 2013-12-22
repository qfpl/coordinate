module Data.GPS.Coordinate.Coordinate(
  Coordinate
, HasCoordinate(..)
, coordinateLatLon
, coordinateLonLat
, coordinateDMSLatLon
, coordinateLatDMSLon
, coordinateDMSLatDMSLon
) where

import Prelude(Eq, Show, id, (.))
import Control.Lens(Iso', Lens', iso, lens, mapping, swapped, withIso)
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

coordinateLatLon ::
  Iso' (Latitude, Longitude) Coordinate
coordinateLatLon =
  iso (\(lat, lon) -> Coordinate lat lon) (\(Coordinate lat lon) -> (lat, lon))

coordinateLonLat ::
  Iso' (Longitude, Latitude) Coordinate
coordinateLonLat =
  swapped . coordinateLatLon

coordinateDMSLatLon ::
  Iso' ((DegreesLatitude, Minutes, Seconds), Longitude) Coordinate
coordinateDMSLatLon =
  swapped . mapping dmsLatitude . coordinateLonLat

coordinateLatDMSLon ::
  Iso' (Latitude, (DegreesLongitude, Minutes, Seconds)) Coordinate
coordinateLatDMSLon =
  mapping dmsLongitude . coordinateLatLon

coordinateDMSLatDMSLon ::
  Iso' ((DegreesLatitude, Minutes, Seconds), (DegreesLongitude, Minutes, Seconds)) Coordinate
coordinateDMSLatDMSLon =
  iso (\((td, tm, ts), (nd, nm, ns)) -> Coordinate (withIso dmsLatitude (\k _ -> k (td, tm, ts))) (withIso dmsLongitude (\k _ -> k (nd, nm, ns))))
      (\(Coordinate lat lon) -> (withIso dmsLatitude (\_ k -> k lat), withIso dmsLongitude (\_ k -> k lon)))

class HasCoordinate t where
  coordinate ::
    Lens' t Coordinate

instance HasCoordinate Coordinate where
  coordinate =
    id

instance HasLatitude Coordinate where
  latitude =
    lens (\(Coordinate lat _) -> lat) (\(Coordinate _ lon) lat -> Coordinate lat lon)

instance HasLongitude Coordinate where
  longitude =
    lens (\(Coordinate _ lon) -> lon) (\(Coordinate lat _) lon -> Coordinate lat lon)

instance HasDegreesLatitude Coordinate where
  degreesLatitude =
    latitude . degreesLatitude

instance HasDegreesLongitude Coordinate where
  degreesLongitude =
    longitude . degreesLongitude
