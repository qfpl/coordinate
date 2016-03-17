{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Data.Geo.Coordinate.Longitude(
  Longitude
, AsLongitude(..)
, modLongitude
, antipodeLongitude
) where

import Control.Applicative(Applicative)
import Control.Category(Category(id))
import Control.Lens(Choice, Profunctor, Optic', Iso', iso, prism', lens, (#), (^?))
import Control.Monad(Monad(return))
import Data.Eq(Eq)
import Data.Fixed(divMod')
import Data.Functor(Functor)
import Data.Geo.Coordinate.DegreesLongitude(DegreesLongitude, AsDegreesLongitude(_DegreesLongitude), modDegreesLongitude, antipodeDegreesLongitude)
import Data.Geo.Coordinate.Minutes(AsMinutes(_Minutes), Minutes, modMinutes)
import Data.Geo.Coordinate.Seconds(AsSeconds(_Seconds), Seconds, modSeconds)
import Data.Ord(Ord((<)))
import Prelude(Double, Show, Int, Num((+), (*), (-), abs), Fractional((/)), properFraction, fromIntegral)

-- $setup
-- >>> import Prelude(Functor(..))
-- >>> import Data.Maybe

data Longitude =
  Longitude
    DegreesLongitude
    Minutes
    Seconds
  deriving (Eq, Ord, Show)

class AsLongitude p f s where
  _Longitude ::
    Optic' p f s Longitude

instance AsLongitude p f Longitude where
  _Longitude =
    id

-- | An isomorphism on the triple of degrees longitude, minutes, seconds to a longitude.
--
-- >>> do deg <- (7 :: Int) ^? _DegreesLongitude; min <- (7 :: Int) ^? _Minutes; sec <- (7 :: Double) ^? _Seconds; (deg, min, sec) ^? _Longitude :: Maybe Longitude
-- Just (Longitude (DegreesLongitude 7) (Minutes 7) (Seconds 7.0000))
--
-- >>> do deg <- (179 :: Int) ^? _DegreesLongitude; min <- (59 :: Int) ^? _Minutes; sec <- (59 :: Double) ^? _Seconds; (deg, min, sec) ^? _Longitude :: Maybe Longitude
-- Just (Longitude (DegreesLongitude 179) (Minutes 59) (Seconds 59.0000))
--
-- >>> do deg <- (-7 :: Int) ^? _DegreesLongitude; min <- (7 :: Int) ^? _Minutes; sec <- (7 :: Double) ^? _Seconds; (deg, min, sec) ^? _Longitude :: Maybe Longitude
-- Just (Longitude (DegreesLongitude (-7)) (Minutes 7) (Seconds 7.0000))
--
-- >>> do deg <- (-179 :: Int) ^? _DegreesLongitude; min <- (59 :: Int) ^? _Minutes; sec <- (59 :: Double) ^? _Seconds; (deg, min, sec) ^? _Longitude :: Maybe Longitude
-- Just (Longitude (DegreesLongitude (-179)) (Minutes 59) (Seconds 59.0000))
--
-- >>> do deg <- (180 :: Int) ^? _DegreesLongitude; min <- (59 :: Int) ^? _Minutes; sec <- (59 :: Double) ^? _Seconds; (deg, min, sec) ^? _Longitude :: Maybe Longitude
-- Nothing
--
-- >>> do deg <- (179 :: Int) ^? _DegreesLongitude; min <- (60 :: Int) ^? _Minutes; sec <- (59 :: Double) ^? _Seconds; (deg, min, sec) ^? _Longitude :: Maybe Longitude
-- Nothing
--
-- >>> do deg <- (179 :: Int) ^? _DegreesLongitude; min <- (59 :: Int) ^? _Minutes; sec <- (60 :: Double) ^? _Seconds; (deg, min, sec) ^? _Longitude :: Maybe Longitude
-- Nothing
--
-- >>> fmap (\x -> _Longitude # x :: (DegreesLongitude, Minutes, Seconds))  ((7 :: Double) ^? _Longitude :: Maybe Longitude)
-- Just (DegreesLongitude 7,Minutes 0,Seconds 0.0000)
--
-- >>> fmap (\x -> _Longitude # x :: (DegreesLongitude, Minutes, Seconds))  ((7.12 :: Double) ^? _Longitude :: Maybe Longitude)
-- Just (DegreesLongitude 7,Minutes 7,Seconds 12.0000)
instance (Profunctor p, Functor f) => AsLongitude p f (DegreesLongitude, Minutes, Seconds) where
  _Longitude =
    iso (\(d, m, s) -> Longitude d m s) (\(Longitude d m s) -> (d, m, s))

-- | A prism on longitude to a double between -180 and 180 exclusive.
--
-- >>> (7 :: Double) ^? _Longitude :: Maybe Longitude
-- Just (Longitude (DegreesLongitude 7) (Minutes 0) (Seconds 0.0000))
--
-- >>> (-7 :: Double) ^? _Longitude :: Maybe Longitude
-- Just (Longitude (DegreesLongitude (-7)) (Minutes 0) (Seconds 0.0000))
--
-- >>> (7.12 :: Double) ^? _Longitude :: Maybe Longitude
-- Just (Longitude (DegreesLongitude 7) (Minutes 7) (Seconds 12.0000))
--
-- >>> (-7.12 :: Double) ^? _Longitude :: Maybe Longitude
-- Just (Longitude (DegreesLongitude (-7)) (Minutes 7) (Seconds 12.0000))
--
-- >>> fmap (\x -> _Longitude # x  :: Double) ((7.12 :: Double) ^? _Longitude :: Maybe Longitude)
-- Just 7.12
--
-- >>> fmap (\x -> _Longitude # x :: Double) ((-7.12 :: Double) ^? _Longitude :: Maybe Longitude)
-- Just (-7.12)
--
-- >>> (180 :: Double) ^? _Longitude :: Maybe Longitude
-- Nothing
--
-- >>> (-180 :: Double) ^? _Longitude :: Maybe Longitude
-- Nothing
--
-- >>> (15.63791 :: Double) ^? _Longitude :: Maybe Longitude
-- Just (Longitude (DegreesLongitude 15) (Minutes 38) (Seconds 16.4760))
--
-- >>> (179.1 :: Double) ^? _Longitude :: Maybe Longitude
-- Just (Longitude (DegreesLongitude 179) (Minutes 5) (Seconds 60.0000))
--
-- >>> (179.2 :: Double) ^? _Longitude :: Maybe Longitude
-- Just (Longitude (DegreesLongitude 179) (Minutes 11) (Seconds 60.0000))
--
-- >>> fmap (\x -> _Longitude # x :: Double) (do deg <- (7 :: Int) ^? _DegreesLongitude; min <- (7 :: Int) ^? _Minutes; sec <- (7 :: Double) ^? _Seconds; (deg, min, sec) ^? _Longitude :: Maybe Longitude)
-- Just 7.118611111111111
--
-- >>> fmap (\x -> _Longitude # x :: Double) (do deg <- (179 :: Int) ^? _DegreesLongitude; min <- (15 :: Int) ^? _Minutes; sec <- (6 :: Double) ^? _Seconds; (deg, min, sec) ^? _Longitude :: Maybe Longitude)
-- Just 179.25166666666667
instance (Choice p, Applicative f) => AsLongitude p f Double where
  _Longitude =
    prism' (\(Longitude d m s) ->
      let p = fromIntegral (_DegreesLongitude # d :: Int)
          q = (fromIntegral (_Minutes # m :: Int) / 60) + (_Seconds # s) / 3600
      in (if p < 0 then (-) else (+)) p q)
      (\x -> let (d, z) = properFraction x
                 (m, s) = properFraction ((z :: Double) * 60)
             in do d' <- (d :: Int) ^? _DegreesLongitude
                   m' <- (abs m :: Int) ^? _Minutes
                   s' <- (abs s * 60) ^? _Seconds
                   return (Longitude d' m' s'))

instance (p ~ (->), Functor f) => AsDegreesLongitude p f Longitude where
  _DegreesLongitude =
    lens (\(Longitude d _ _) -> d) (\(Longitude _ m s) d -> Longitude d m s)
    
instance (p ~ (->), Functor f) => AsMinutes p f Longitude where
  _Minutes =
    lens (\(Longitude _ m _) -> m) (\(Longitude d _ s) m -> Longitude d m s)

instance (p ~ (->), Functor f) => AsSeconds p f Longitude where
  _Seconds =
    lens (\(Longitude _ _ s) -> s) (\(Longitude d m _) s -> Longitude d m s)

-- | Setting a longitude using modulo arithmetic.
--
-- >>> modLongitude 20 20 20
-- Longitude (DegreesLongitude 20) (Minutes 20) (Seconds 20.0000)
--
-- >>> modLongitude 20 20 (-20)
-- Longitude (DegreesLongitude 20) (Minutes 19) (Seconds 40.0000)
--
-- >>> modLongitude 20 20 80
-- Longitude (DegreesLongitude 20) (Minutes 21) (Seconds 20.0000)
--
-- >>> modLongitude 20 80 20
-- Longitude (DegreesLongitude 21) (Minutes 20) (Seconds 20.0000)
--
-- >>> modLongitude 20 80 80
-- Longitude (DegreesLongitude 21) (Minutes 21) (Seconds 20.0000)
--
-- >>> modLongitude 20 80 (-20)
-- Longitude (DegreesLongitude 21) (Minutes 19) (Seconds 40.0000)
--
-- >>> modLongitude 200 20 20
-- Longitude (DegreesLongitude (-160)) (Minutes 20) (Seconds 20.0000)
--
-- >>> modLongitude 200 20 80
-- Longitude (DegreesLongitude (-160)) (Minutes 21) (Seconds 20.0000)
--
-- >>> modLongitude 200 80 20
-- Longitude (DegreesLongitude (-159)) (Minutes 20) (Seconds 20.0000)
--
-- >>> modLongitude 200 20 (-20)
-- Longitude (DegreesLongitude (-160)) (Minutes 19) (Seconds 40.0000)
--
-- >>> modLongitude 200 (-20) (-20)
-- Longitude (DegreesLongitude (-161)) (Minutes 39) (Seconds 40.0000)
--
-- >>> modLongitude 200 (-80) (-20)
-- Longitude (DegreesLongitude (-162)) (Minutes 39) (Seconds 40.0000)
--
-- >>> modLongitude 20 20 3620
-- Longitude (DegreesLongitude 21) (Minutes 20) (Seconds 20.0000)
--
-- >>> modLongitude 20 20 (-3580)
-- Longitude (DegreesLongitude 19) (Minutes 20) (Seconds 20.0000)
--
-- >>> modLongitude 200 20 3620
-- Longitude (DegreesLongitude (-159)) (Minutes 20) (Seconds 20.0000)
--
-- >>> modLongitude 200 20 (-3580)
-- Longitude (DegreesLongitude (-161)) (Minutes 20) (Seconds 20.0000)
modLongitude ::
  Int
  -> Int
  -> Double
  -> Longitude
modLongitude d m s =
  let (ts, rs) = s `divMod'` 60
      (tm, rm) = (ts + m) `divMod'` 60
  in Longitude (modDegreesLongitude (tm + d)) (modMinutes rm) (modSeconds rs)

-- | The longitude that is symmetrical around the prime meridian.
--
-- >>> fmap (\x -> antipodeLongitude # x) (do d <- (7 :: Int) ^? _DegreesLongitude; m <- (7 :: Int) ^? _Minutes; s <- (7 :: Double) ^? _Seconds; (d, m, s) ^? _Longitude :: Maybe Longitude)
-- Just (Longitude (DegreesLongitude (-7)) (Minutes 7) (Seconds 7.0000))
antipodeLongitude ::
  Iso'
    Longitude
    Longitude
antipodeLongitude =
  let n (Longitude d m s) = Longitude (antipodeDegreesLongitude # d) m s
  in  iso
        n
        n
