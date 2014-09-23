{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Data.Geo.Coordinate.Coordinate(
  Coordinate
, (.#.)
, (<°>)
, (<㎭>)
, fracCoordinate
, radianCoordinate
, HasCoordinate(..)
, coordinateLatLon
, coordinateLonLat
, coordinateDMSLatLon
, coordinateLatDMSLon
, coordinateDMSLatDMSLon
) where

import Control.Category(Category(id, (.)))
import Control.Lens(Iso', Lens', Prism', iso, lens, prism', mapping, swapped, withIso, (^?), (#))
import Control.Monad(Monad(return))
import Data.Eq(Eq)
import Data.Geo.Coordinate.Latitude(HasLatitude(latitude), Latitude, dmsLatitude, radianLatitude, fracLatitude)
import Data.Geo.Coordinate.Longitude(HasLongitude(longitude), Longitude, dmsLongitude, radianLongitude, fracLongitude)
import Data.Geo.Coordinate.DegreesLatitude(HasDegreesLatitude(degreesLatitude), DegreesLatitude)
import Data.Geo.Coordinate.DegreesLongitude(HasDegreesLongitude(degreesLongitude), DegreesLongitude)
import Data.Geo.Coordinate.Minutes(Minutes)
import Data.Geo.Coordinate.Seconds(Seconds)
import Data.Maybe(Maybe)
import Data.Ord(Ord)
import Data.Tuple(curry)
import Prelude(Show, Double)

data Coordinate =
  Coordinate
    Latitude
    Longitude
  deriving (Eq, Ord, Show)

-- | Build a coordinate from a latitude and longitude.
(.#.) ::
  Latitude
  -> Longitude
  -> Coordinate
(.#.) =
  Coordinate

-- | Build a coordinate from a fractional latitude and fractional longitude. Fails
-- if either are out of range.
(<°>) ::
  Double
  -> Double
  -> Maybe Coordinate
(<°>) =
  curry (^? fracCoordinate)

-- | Build a coordinate from a radian latitude and fractional longitude. Fails
-- if either are out of range.
(<㎭>) ::
  Double
  -> Double
  -> Maybe Coordinate
(<㎭>) =
  curry (^? radianCoordinate)
 
coordinatePrism ::
  Prism' Double Latitude
  -> Prism' Double Longitude
  -> Prism' (Double, Double) Coordinate
coordinatePrism f g =
  prism'
    (\x -> let (b, d) = coordinateLatLon # x
           in (f # b, g # d))
    (\(lat, lon) ->
      do lat' <- lat ^? f
         lon' <- lon ^? g
         return (Coordinate lat' lon'))

-- | A prism on the pair (below) to a coordinate:
-- 
-- * a fractional latitude to a double between -90 and 90 exclusive.
--
-- * a fractional longitude to a double between -180 and 180 exclusive.
fracCoordinate ::
  Prism' (Double, Double) Coordinate
fracCoordinate =
  coordinatePrism fracLatitude fracLongitude

-- | A prism on the pair (below) to a coordinate:
-- 
-- * a radian latitude to a double between -π/2 and π/2 exclusive.
--
-- * a radian longitude to a double between -π and π exclusive.
radianCoordinate ::
  Prism' (Double, Double) Coordinate
radianCoordinate =
  coordinatePrism radianLatitude radianLongitude
  
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
