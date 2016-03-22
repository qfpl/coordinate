{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Data.Geo.Coordinate.Latitude(
  Latitude
, AsLatitude(..)
, modLatitude
, modLatitude'
, antipodeLatitude
) where

import Control.Applicative(Applicative)
import Control.Category(Category(id))
import Control.Lens(Choice, Profunctor, Optic', Iso', iso, prism', lens, (#), (^?))
import Control.Monad(Monad(return))
import Data.Eq(Eq((==)))
import Data.Fixed(divMod', mod')
import Data.Functor(Functor)
import Data.Geo.Coordinate.DegreesLatitude(DegreesLatitude, AsDegreesLatitude(_DegreesLatitude), modDegreesLatitude, antipodeDegreesLatitude)
import Data.Geo.Coordinate.Minutes(AsMinutes(_Minutes), Minutes, modMinutes)
import Data.Geo.Coordinate.Seconds(AsSeconds(_Seconds), Seconds, modSeconds)
import Data.Ord(Ord((<)))
import Prelude(Double, Show, Int, Num((+), (*), (-), abs), Fractional((/)), properFraction, fromIntegral)

-- $setup
-- >>> import Prelude(Functor(..))
-- >>> import Data.Maybe

data Latitude =
  Latitude
    DegreesLatitude
    Minutes
    Seconds
  deriving (Eq, Ord, Show)

class AsLatitude p f s where
  _Latitude ::
    Optic' p f s Latitude

instance AsLatitude p f Latitude where
  _Latitude =
    id

-- | An isomorphism on the triple of degrees latitude, minutes, seconds to a latitude.
--
-- >>> do deg <- (7 :: Int) ^? _DegreesLatitude; min <- (7 :: Int) ^? _Minutes; sec <- (7 :: Double) ^? _Seconds; (deg, min, sec) ^? _Latitude :: Maybe Latitude
-- Just (Latitude (DegreesLatitude 7) (Minutes 7) (Seconds 7.0000))
--
-- >>> do deg <- (89 :: Int) ^? _DegreesLatitude; min <- (59 :: Int) ^? _Minutes; sec <- (59 :: Double) ^? _Seconds; (deg, min, sec) ^? _Latitude :: Maybe Latitude
-- Just (Latitude (DegreesLatitude 89) (Minutes 59) (Seconds 59.0000))
--
-- >>> do deg <- (-7 :: Int) ^? _DegreesLatitude; min <- (7 :: Int) ^? _Minutes; sec <- (7 :: Double) ^? _Seconds; (deg, min, sec) ^? _Latitude :: Maybe Latitude
-- Just (Latitude (DegreesLatitude (-7)) (Minutes 7) (Seconds 7.0000))
--
-- >>> do deg <- (-89 :: Int) ^? _DegreesLatitude; min <- (59 :: Int) ^? _Minutes; sec <- (59 :: Double) ^? _Seconds; (deg, min, sec) ^? _Latitude :: Maybe Latitude
-- Just (Latitude (DegreesLatitude (-89)) (Minutes 59) (Seconds 59.0000))
--
-- >>> do deg <- (90 :: Int) ^? _DegreesLatitude; min <- (59 :: Int) ^? _Minutes; sec <- (59 :: Double) ^? _Seconds; (deg, min, sec) ^? _Latitude :: Maybe Latitude
-- Nothing
--
-- >>> do deg <- (89 :: Int) ^? _DegreesLatitude; min <- (60 :: Int) ^? _Minutes; sec <- (59 :: Double) ^? _Seconds; (deg, min, sec) ^? _Latitude :: Maybe Latitude
-- Nothing
--
-- >>> do deg <- (89 :: Int) ^? _DegreesLatitude; min <- (59 :: Int) ^? _Minutes; sec <- (60 :: Double) ^? _Seconds; (deg, min, sec) ^? _Latitude :: Maybe Latitude
-- Nothing
--
-- >>> fmap (\x -> _Latitude # x :: (DegreesLatitude, Minutes, Seconds))  ((7 :: Double) ^? _Latitude :: Maybe Latitude)
-- Just (DegreesLatitude 7,Minutes 0,Seconds 0.0000)
--
-- >>> fmap (\x -> _Latitude # x :: (DegreesLatitude, Minutes, Seconds))  ((7.12 :: Double) ^? _Latitude :: Maybe Latitude)
-- Just (DegreesLatitude 7,Minutes 7,Seconds 12.0000)
instance (Profunctor p, Functor f) => AsLatitude p f (DegreesLatitude, Minutes, Seconds) where
  _Latitude =
    iso (\(d, m, s) -> Latitude d m s) (\(Latitude d m s) -> (d, m, s))

-- | A prism on latitude to a double between -90 and 90 exclusive.
--
-- >>> (7 :: Double) ^? _Latitude :: Maybe Latitude
-- Just (Latitude (DegreesLatitude 7) (Minutes 0) (Seconds 0.0000))
--
-- >>> (-7 :: Double) ^? _Latitude :: Maybe Latitude
-- Just (Latitude (DegreesLatitude (-7)) (Minutes 0) (Seconds 0.0000))
--
-- >>> (7.12 :: Double) ^? _Latitude :: Maybe Latitude
-- Just (Latitude (DegreesLatitude 7) (Minutes 7) (Seconds 12.0000))
--
-- >>> (-7.12 :: Double) ^? _Latitude :: Maybe Latitude
-- Just (Latitude (DegreesLatitude (-7)) (Minutes 7) (Seconds 12.0000))
--
-- >>> fmap (\x -> _Latitude # x :: Double) ((7.12 :: Double) ^? _Latitude :: Maybe Latitude)
-- Just 7.12
--
-- >>> fmap (\x -> _Latitude # x :: Double) ((-7.12 :: Double) ^? _Latitude :: Maybe Latitude)
-- Just (-7.12)
--
-- >>> (90 :: Double) ^? _Latitude :: Maybe Latitude
-- Nothing
--
-- >>> (-90 :: Double) ^? _Latitude :: Maybe Latitude
-- Nothing
--
-- >>> (15.63791 :: Double) ^? _Latitude :: Maybe Latitude
-- Just (Latitude (DegreesLatitude 15) (Minutes 38) (Seconds 16.4760))
--
-- >>> (89.1 :: Double) ^? _Latitude :: Maybe Latitude
-- Just (Latitude (DegreesLatitude 89) (Minutes 5) (Seconds 60.0000))
--
-- >>> (89.2 :: Double) ^? _Latitude :: Maybe Latitude
-- Just (Latitude (DegreesLatitude 89) (Minutes 12) (Seconds 0.0000))
--
-- >>> fmap (\x -> _Latitude # x :: Double) (do deg <- (7 :: Int) ^? _DegreesLatitude; min <- (7 :: Int) ^? _Minutes; sec <- (7 :: Double) ^? _Seconds; (deg, min, sec) ^? _Latitude :: Maybe Latitude)
-- Just 7.118611111111111
--
-- >>> fmap (\x -> _Latitude # x  :: Double) (do deg <- (89 :: Int) ^? _DegreesLatitude; min <- (15 :: Int) ^? _Minutes; sec <- (6 :: Double) ^? _Seconds; (deg, min, sec) ^? _Latitude :: Maybe Latitude)
-- Just 89.25166666666667
instance (Choice p, Applicative f) => AsLatitude p f Double where
  _Latitude =
    prism' (\(Latitude d m s) ->
    let p = fromIntegral (_DegreesLatitude # d :: Int)
        q = (fromIntegral (_Minutes # m :: Int) / 60) + (_Seconds # s) / 3600
    in (if p < 0 then (-) else (+)) p q)
    (\x -> let (d, z) = properFraction x
               (m, s) = properFraction ((z :: Double) * 60)
           in do d' <- (d :: Int) ^? _DegreesLatitude
                 m' <- (abs m :: Int) ^? _Minutes
                 s' <- (abs s * 60) ^? _Seconds
                 return (Latitude d' m' s'))

instance (p ~ (->), Functor f) => AsDegreesLatitude p f Latitude where
  _DegreesLatitude =
    lens (\(Latitude d _ _) -> d) (\(Latitude _ m s) d -> Latitude d m s)

instance (p ~ (->), Functor f) => AsMinutes p f Latitude where
  _Minutes =
    lens (\(Latitude _ m _) -> m) (\(Latitude d _ s) m -> Latitude d m s)

instance (p ~ (->), Functor f) => AsSeconds p f Latitude where
  _Seconds =
    lens (\(Latitude _ _ s) -> s) (\(Latitude d m _) s -> Latitude d m s)

-- | Setting a latitude using modulo arithmetic.
--
-- >>> modLatitude 20 20 20
-- Latitude (DegreesLatitude 20) (Minutes 20) (Seconds 20.0000)
--
-- >>> modLatitude 20 20 (-20)
-- Latitude (DegreesLatitude 20) (Minutes 19) (Seconds 40.0000)
--
-- >>> modLatitude 20 20 80
-- Latitude (DegreesLatitude 20) (Minutes 21) (Seconds 20.0000)
--
-- >>> modLatitude 20 80 20
-- Latitude (DegreesLatitude 21) (Minutes 20) (Seconds 20.0000)
--
-- >>> modLatitude 20 80 80
-- Latitude (DegreesLatitude 21) (Minutes 21) (Seconds 20.0000)
--
-- >>> modLatitude 20 80 (-20)
-- Latitude (DegreesLatitude 21) (Minutes 19) (Seconds 40.0000)
--
-- >>> modLatitude 110 20 20
-- Latitude (DegreesLatitude (-70)) (Minutes 20) (Seconds 20.0000)
--
-- >>> modLatitude 110 20 80
-- Latitude (DegreesLatitude (-70)) (Minutes 21) (Seconds 20.0000)
--
-- >>> modLatitude 110 80 20
-- Latitude (DegreesLatitude (-69)) (Minutes 20) (Seconds 20.0000)
--
-- >>> modLatitude 110 20 (-20)
-- Latitude (DegreesLatitude (-70)) (Minutes 19) (Seconds 40.0000)
--
-- >>> modLatitude 110 (-20) (-20)
-- Latitude (DegreesLatitude (-71)) (Minutes 39) (Seconds 40.0000)
--
-- >>> modLatitude 110 (-80) (-20)
-- Latitude (DegreesLatitude (-72)) (Minutes 39) (Seconds 40.0000)
--
-- >>> modLatitude 20 20 3620
-- Latitude (DegreesLatitude 21) (Minutes 20) (Seconds 20.0000)
--
-- >>> modLatitude 20 20 (-3580)
-- Latitude (DegreesLatitude 19) (Minutes 20) (Seconds 20.0000)
--
-- >>> modLatitude 110 20 3620
-- Latitude (DegreesLatitude (-69)) (Minutes 20) (Seconds 20.0000)
--
-- >>> modLatitude 110 20 (-3580)
-- Latitude (DegreesLatitude (-71)) (Minutes 20) (Seconds 20.0000)
modLatitude ::
  Int
  -> Int
  -> Double
  -> Latitude
modLatitude d m s =
  let (ts, rs) = s `divMod'` 60
      (tm, rm) = (ts + m) `divMod'` 60
      zd = modDegreesLatitude (tm + d)
      zm = modMinutes rm
      zs = modSeconds rs
  in Latitude undefined zm zs

modLatitude' ::
  Double
  -> Latitude
modLatitude' x =
  let x' = if x == 90 then 90 else mod' (x + 90) 180 - 90
      (d, z) = properFraction x
      (m, s) = properFraction ((z :: Double) * 60)
  in  modLatitude d (abs m) (abs s * 60)

undefined = undefined

-- | The latitude that is symmetrical around the equator.
--
-- >>> fmap (\x -> antipodeLatitude # x) (do d <- (7 :: Int) ^? _DegreesLatitude; m <- (7 :: Int) ^? _Minutes; s <- (7 :: Double) ^? _Seconds; (d, m, s) ^? _Latitude :: Maybe Latitude)
-- Just (Latitude (DegreesLatitude (-7)) (Minutes 7) (Seconds 7.0000))
antipodeLatitude ::
  Iso'
    Latitude
    Latitude
antipodeLatitude =
  let n (Latitude d m s) = Latitude (antipodeDegreesLatitude # d) m s
  in  iso
        n
        n
