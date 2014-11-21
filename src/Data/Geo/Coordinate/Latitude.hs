{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Data.Geo.Coordinate.Latitude(
  Latitude
, AsLatitude(..)
) where

import Control.Applicative(Applicative)
import Control.Category(Category(id, (.)))
import Control.Lens(Choice, Profunctor, Optic', iso, prism', lens, (#), (^?))
import Control.Monad(Monad(return))
import Data.Eq(Eq)
import Data.Functor(Functor)
import Data.Geo.Coordinate.DegreesLatitude(DegreesLatitude, AsDegreesLatitude(_DegreesLatitude))
import Data.Geo.Coordinate.Minutes(AsMinutes(_Minutes), Minutes)
import Data.Geo.Coordinate.Seconds(AsSeconds(_Seconds), Seconds)
import Data.Ord(Ord((<)))
import Data.Radian(Radian, radians)
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

-- | A prism on latitude to a double between -π2 and π2 exclusive. 
--
-- >>> (0.2 :: Radian Double) ^? _Latitude
-- Just (Latitude (DegreesLatitude 11) (Minutes 27) (Seconds 32.9612))
--
-- >>> (1.3 :: Radian Double) ^? _Latitude
-- Just (Latitude (DegreesLatitude 74) (Minutes 29) (Seconds 4.2481))
--
-- >>> (-1.3 :: Radian Double) ^? _Latitude
-- Just (Latitude (DegreesLatitude (-74)) (Minutes 29) (Seconds 4.2481))
--
-- >>> (1.5707963 :: Radian Double) ^? _Latitude
-- Just (Latitude (DegreesLatitude 89) (Minutes 59) (Seconds 59.9945))
--
-- >>> (1.58 :: Radian Double) ^? _Latitude
-- Nothing
--
-- >>> (-1.58 :: Radian Double) ^? _Latitude
-- Nothing
--
-- >>> fmap (\x -> _Latitude # x  :: Radian Double) (do deg <- (7 :: Int) ^? _DegreesLatitude; min <- (7 :: Int) ^? _Minutes; sec <- (7 :: Double) ^? _Seconds; (deg, min, sec) ^? _Latitude :: Maybe Latitude)
-- Just (Radian 0.12424320205794079)
--
-- >>> fmap (\x -> _Latitude # x  :: Radian Double) (do deg <- (89 :: Int) ^? _DegreesLatitude; min <- (15 :: Int) ^? _Minutes; sec <- (6 :: Double) ^? _Seconds; (deg, min, sec) ^? _Latitude :: Maybe Latitude)
-- Just (Radian 1.5577354462258055)
instance (Choice p, Applicative f) => AsLatitude p f (Radian Double) where
  _Latitude =
    radians . _Latitude

instance (p ~ (->), Functor f) => AsDegreesLatitude p f Latitude where
  _DegreesLatitude =
    lens (\(Latitude d _ _) -> d) (\(Latitude _ m s) d -> Latitude d m s)

instance (p ~ (->), Functor f) => AsMinutes p f Latitude where
  _Minutes =
    lens (\(Latitude _ m _) -> m) (\(Latitude d _ s) m -> Latitude d m s)

instance (p ~ (->), Functor f) => AsSeconds p f Latitude where
  _Seconds =
    lens (\(Latitude _ _ s) -> s) (\(Latitude d m _) s -> Latitude d m s)
