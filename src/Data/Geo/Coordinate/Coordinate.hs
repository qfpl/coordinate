{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Data.Geo.Coordinate.Coordinate(
  Coordinate
, AsCoordinate(..)
, latitudeMinutes
, latitudeSeconds
, longitudeMinutes
, longitudeSeconds
, (.#.)
, (<°>)
, antipode
) where

import Control.Applicative(Applicative((<*>)))
import Control.Category(Category(id, (.)))
import Control.Lens(Identity, Const, Prism', Choice, Iso', swapped, Profunctor, Optic', (^.), iso, from, lens, prism', swapped, (^?), (#))
import Control.Monad(Monad(return))
import Data.Eq(Eq)
import Data.Functor(Functor, (<$>))
import Data.Geo.Coordinate.Latitude(AsLatitude(_Latitude), Latitude, antipodeLatitude)
import Data.Geo.Coordinate.Longitude(AsLongitude(_Longitude), Longitude, antipodeLongitude)
import Data.Geo.Coordinate.DegreesLatitude(AsDegreesLatitude(_DegreesLatitude), DegreesLatitude)
import Data.Geo.Coordinate.DegreesLongitude(AsDegreesLongitude(_DegreesLongitude), DegreesLongitude)
import Data.Geo.Coordinate.Minutes(Minutes, AsMinutes(_Minutes))
import Data.Geo.Coordinate.Seconds(Seconds, AsSeconds(_Seconds))
import Data.Maybe(Maybe)
import Data.Monoid(First)
import Data.Ord(Ord)
import Data.Tagged(Tagged)
import Data.Tuple(curry, uncurry)
import Prelude(Show, Double)

data Coordinate =
  Coordinate
    Latitude
    Longitude
  deriving (Eq, Ord, Show)

-- | Build a coordinate from a latitude and longitude.
(.#.) ::
  (  
    AsLatitude (->) (Const Latitude) lat
  , AsLongitude (->) (Const Longitude) lon
  ) =>
  lat
  -> lon
  -> Coordinate
lat .#. lon =
  Coordinate
    (lat ^. _Latitude)
    (lon ^. _Longitude)

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

coordinatePrism' ::
  (
    AsLatitude Tagged Identity lat
  , AsLatitude (->) (Const (First Latitude)) lat
  , AsLongitude Tagged Identity lon
  , AsLongitude (->) (Const (First Longitude)) lon) =>
  Prism' (lat, lon) Coordinate
coordinatePrism' =
  coordinatePrism (_Latitude #) (_Longitude #) (^? _Latitude) (^? _Longitude)

coordinatePrism ::
  (Latitude -> lat)
  -> (Longitude -> lon)
  -> (lat -> Maybe Latitude)
  -> (lon -> Maybe Longitude)
  -> Prism' (lat, lon) Coordinate
coordinatePrism f g h i =
  prism'
      (\(Coordinate lat lon) -> (f lat, g lon))
      (\(lat, lon) ->
        do lat' <- h lat
           lon' <- i lon
           return (Coordinate lat' lon'))

instance (Choice p, Applicative f) => AsCoordinate p f (Double, Double) where
  _Coordinate =
    coordinatePrism'

instance (Choice p, Applicative f) => AsCoordinate p f (Latitude, Double) where
  _Coordinate =
    coordinatePrism id (_Longitude #) return (^? _Longitude)
    
instance (Choice p, Applicative f) => AsCoordinate p f (Double, Longitude) where
  _Coordinate =
    coordinatePrism (_Latitude #) id (^? _Latitude) return

instance (Choice p, Applicative f) => AsCoordinate p f ((DegreesLatitude, Minutes, Seconds), Double) where
  _Coordinate =
    coordinatePrism'

instance (Choice p, Applicative f) => AsCoordinate p f (Double, (DegreesLongitude, Minutes, Seconds)) where
  _Coordinate =
    coordinatePrism'

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

instance (p ~ (->), Applicative f) => AsMinutes p f Coordinate where
  _Minutes f (Coordinate lat lon) =
    (\m1 m2 ->
      Coordinate
        (from _Latitude # (lat ^. _DegreesLatitude, m1, lat ^. _Seconds))
        (from _Longitude # (lon ^. _DegreesLongitude, m2, lon ^. _Seconds))) <$> 
    f (lat ^. _Minutes) <*> 
    f (lon ^. _Minutes)
    
instance (p ~ (->), Applicative f) => AsSeconds p f Coordinate where
  _Seconds f (Coordinate lat lon) =
    (\s1 s2 ->
      Coordinate
        (from _Latitude # (lat ^. _DegreesLatitude, lat ^. _Minutes, s1))
        (from _Longitude # (lon ^. _DegreesLongitude, lon ^. _Minutes, s2))) <$> 
    f (lat ^. _Seconds) <*> 
    f (lon ^. _Seconds)

latitudeMinutes ::  
  (AsCoordinate (->) f coord, Functor f) =>
  Optic' (->) f coord Minutes  
latitudeMinutes =
  _Coordinate . _Latitude . _Minutes

latitudeSeconds ::
  (AsCoordinate (->) f coord, Functor f) =>
  Optic' (->) f coord Seconds
latitudeSeconds =
  _Coordinate . _Latitude . _Seconds

longitudeMinutes ::
  (AsCoordinate (->) f coord, Functor f) =>
  Optic' (->) f coord Minutes  
longitudeMinutes =
  _Coordinate . _Longitude . _Minutes

longitudeSeconds ::
  (AsCoordinate (->) f coord, Functor f) =>
  Optic' (->) f coord Seconds
longitudeSeconds =
  _Coordinate . _Longitude . _Seconds

antipode ::
  Iso'
    Coordinate
    Coordinate
antipode =
  let n (Coordinate lat lon) = Coordinate (antipodeLatitude # lat) (antipodeLongitude # lon)
  in  iso
        n
        n
