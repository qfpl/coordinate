{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Data.Geo.Coordinate.Coordinate(
  Coordinate
, AsCoordinate(..)
, (.#.)
, (<°>)
) where

import Control.Applicative(Applicative)
import Control.Category(Category(id, (.)))
import Control.Lens(Choice, swapped, Profunctor, Optic', (^.), iso, lens, prism', swapped, (^?), (#))
import Control.Monad(Monad(return))
import Data.Eq(Eq)
import Data.Geo.Coordinate.Latitude(AsLatitude(_Latitude), Latitude)
import Data.Geo.Coordinate.Longitude(AsLongitude(_Longitude), Longitude)
import Data.Geo.Coordinate.DegreesLatitude(AsDegreesLatitude(_DegreesLatitude), DegreesLatitude)
import Data.Geo.Coordinate.DegreesLongitude(AsDegreesLongitude(_DegreesLongitude), DegreesLongitude)
import Data.Geo.Coordinate.Minutes(Minutes)
import Data.Geo.Coordinate.Seconds(Seconds)
import Data.Maybe(Maybe)
import Data.Ord(Ord)
import Data.Tuple(curry, uncurry)
import Prelude(Double, Functor, Show, Double)

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
  curry (^? _Coordinate)

class AsCoordinate p f s where
  _Coordinate ::
    Optic' p f s Coordinate

instance AsCoordinate p f Coordinate where
  _Coordinate =
    id

instance (Profunctor p, Functor f) => AsCoordinate p f (Latitude, Longitude) where
  _Coordinate =
    iso (uncurry Coordinate) (\(Coordinate lat lon) -> (lat, lon))

instance (Profunctor p, Functor f) => AsCoordinate p f (Longitude, Latitude) where
  _Coordinate =
    swapped . _Coordinate

instance (Profunctor p, Functor f) => AsCoordinate p f ((DegreesLatitude, Minutes, Seconds), Longitude) where
  _Coordinate =
    iso
      (\(lat, lon) -> Coordinate (lat ^. _Latitude) lon)
      (\(Coordinate lat lon) -> (_Latitude # lat, lon))

instance (Profunctor p, Functor f) => AsCoordinate p f (Longitude, (DegreesLatitude, Minutes, Seconds)) where
  _Coordinate =
    swapped . _Coordinate

instance (Profunctor p, Functor f) => AsCoordinate p f (Latitude, (DegreesLongitude, Minutes, Seconds)) where
  _Coordinate =
    iso
      (\(lat, lon) -> Coordinate lat (lon ^. _Longitude))
      (\(Coordinate lat lon) -> (lat, _Longitude # lon))

instance (Profunctor p, Functor f) => AsCoordinate p f ((DegreesLongitude, Minutes, Seconds), Latitude) where
  _Coordinate =
    swapped . _Coordinate

instance (Profunctor p, Functor f) => AsCoordinate p f ((DegreesLatitude, Minutes, Seconds), (DegreesLongitude, Minutes, Seconds)) where
  _Coordinate =
    iso
      (\(lat, lon) -> Coordinate (lat ^. _Latitude) (lon ^. _Longitude))
      (\(Coordinate lat lon) -> (_Latitude # lat, _Longitude # lon))

instance (Profunctor p, Functor f) => AsCoordinate p f ((DegreesLongitude, Minutes, Seconds), (DegreesLatitude, Minutes, Seconds)) where
  _Coordinate =
    swapped . _Coordinate

instance (Choice p, Applicative f) => AsCoordinate p f (Double, Double) where
  _Coordinate =
    prism'
      (\(Coordinate lat lon) -> (_Latitude # lat, _Longitude # lon))
      (\(lat, lon) ->
        do lat' <- lat ^? _Latitude
           lon' <- lon ^? _Longitude
           return (Coordinate lat' lon'))

instance (Choice p, Applicative f) => AsCoordinate p f (Latitude, Double) where
  _Coordinate =
    prism'
      (\(Coordinate lat lon) -> (lat, _Longitude # lon))
      (\(lat, lon) ->
        do lon' <- lon ^? _Longitude
           return (Coordinate lat lon'))

instance (Choice p, Applicative f) => AsCoordinate p f (Double, Longitude) where
  _Coordinate =
    prism'
      (\(Coordinate lat lon) -> (_Latitude # lat, lon))
      (\(lat, lon) ->
        do lat' <- lat ^? _Latitude
           return (Coordinate lat' lon))

instance (Choice p, Applicative f) => AsCoordinate p f ((DegreesLatitude, Minutes, Seconds), Double) where
  _Coordinate =
    prism'
      (\(Coordinate lat lon) -> (_Latitude # lat, _Longitude # lon))
      (\(lat, lon) ->
        do lat' <- lat ^? _Latitude
           lon' <- lon ^? _Longitude
           return (Coordinate lat' lon'))

instance (Choice p, Applicative f) => AsCoordinate p f (Double, (DegreesLongitude, Minutes, Seconds)) where
  _Coordinate =
    prism'
      (\(Coordinate lat lon) -> (_Latitude # lat, _Longitude # lon))
      (\(lat, lon) ->
        do lat' <- lat ^? _Latitude
           lon' <- lon ^? _Longitude
           return (Coordinate lat' lon'))

instance (p ~ (->), Functor f) => AsLatitude p f Coordinate where
  _Latitude =
    lens (\(Coordinate lat _) -> lat) (\(Coordinate _ lon) lat -> Coordinate lat lon)
    
instance (p ~ (->), Functor f) => AsDegreesLatitude p f Coordinate where
  _DegreesLatitude =
    _Latitude . _DegreesLatitude

instance (p ~ (->), Functor f) => AsLongitude p f Coordinate where
  _Longitude =
    lens (\(Coordinate _ lon) -> lon) (\(Coordinate lat _) lon -> Coordinate lat lon)
    
instance (p ~ (->), Functor f) => AsDegreesLongitude p f Coordinate where
  _DegreesLongitude =
    _Longitude . _DegreesLongitude
