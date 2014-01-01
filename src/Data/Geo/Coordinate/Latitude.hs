module Data.Geo.Coordinate.Latitude(
  Latitude
, HasLatitude(..)
, dmsLatitude
, fracLatitude
) where

import Prelude(Double, Eq, Show, Ord(..), Num(..), Bool(..), Monad(..), id, (&&), properFraction, fromIntegral)
import Control.Lens(Iso', Prism', Lens', iso, prism', lens, (#), (^?))
import Data.Geo.Coordinate.DegreesLatitude
import Data.Geo.Coordinate.Minutes
import Data.Geo.Coordinate.Seconds

-- $setup
-- >>> import Control.Lens((^?))

data Latitude =
  Latitude
    DegreesLatitude
    Minutes
    Seconds
  deriving (Eq, Ord, Show)

-- | An isomorphism on the triple of degrees latitude, minutes, seconds to a latitude.
--
-- >>> do deg <- 7 ^? nDegreesLatitude; min <- 7 ^? nMinutes; sec <- 7 ^? nSeconds; (deg, min, sec) ^? dmsLatitude
-- Just (Latitude (DegreesLatitude 7) (Minutes 7) (Seconds 7.0000))
--
-- >>> do deg <- 89 ^? nDegreesLatitude; min <- 59 ^? nMinutes; sec <- 59 ^? nSeconds; (deg, min, sec) ^? dmsLatitude
-- Just (Latitude (DegreesLatitude 89) (Minutes 59) (Seconds 59.0000))
--
-- >>> do deg <- (-7) ^? nDegreesLatitude; min <- 7 ^? nMinutes; sec <- 7 ^? nSeconds; (deg, min, sec) ^? dmsLatitude
-- Just (Latitude (DegreesLatitude (-7)) (Minutes 7) (Seconds 7.0000))
--
-- >>> do deg <- (-89) ^? nDegreesLatitude; min <- 59 ^? nMinutes; sec <- 59 ^? nSeconds; (deg, min, sec) ^? dmsLatitude
-- Just (Latitude (DegreesLatitude (-89)) (Minutes 59) (Seconds 59.0000))
--
-- >>> do deg <- 90 ^? nDegreesLatitude; min <- 59 ^? nMinutes; sec <- 59 ^? nSeconds; (deg, min, sec) ^? dmsLatitude
-- Nothing
--
-- >>> do deg <- 89 ^? nDegreesLatitude; min <- 60 ^? nMinutes; sec <- 59 ^? nSeconds; (deg, min, sec) ^? dmsLatitude
-- Nothing
--
-- >>> do deg <- 89 ^? nDegreesLatitude; min <- 59 ^? nMinutes; sec <- 60 ^? nSeconds; (deg, min, sec) ^? dmsLatitude
-- Nothing
dmsLatitude ::
  Iso' (DegreesLatitude, Minutes, Seconds) Latitude
dmsLatitude =
  iso (\(d, m, s) -> Latitude d m s) (\(Latitude d m s) -> (d, m, s))

-- | A prism on latitude to a double between -90 and 90 inclusive.
--
-- >>> 7 ^? fracLatitude
-- Just (Latitude (DegreesLatitude 7) (Minutes 0) (Seconds 0.0000))
--
-- >>> (-7) ^? fracLatitude
-- Just (Latitude (DegreesLatitude (-7)) (Minutes 0) (Seconds 0.0000))
--
-- >>> 7.12 ^? fracLatitude
-- Just (Latitude (DegreesLatitude 7) (Minutes 7) (Seconds 12.0000))
--
-- >>> (-7.12) ^? fracLatitude
-- Just (Latitude (DegreesLatitude (-7)) (Minutes 7) (Seconds 12.0000))
--
-- >>> 90 ^? fracLatitude
-- Nothing
--
-- >>> (-90) ^? fracLatitude
-- Nothing
--
-- >>> 15.63791 ^? fracLatitude
-- Just (Latitude (DegreesLatitude 15) (Minutes 38) (Seconds 16.4760))
--
-- >>> 89.1 ^? fracLatitude
-- Just (Latitude (DegreesLatitude 89) (Minutes 5) (Seconds 60.0000))
--
-- >>> 89.2 ^? fracLatitude
-- Just (Latitude (DegreesLatitude 89) (Minutes 12) (Seconds 0.0000))
fracLatitude ::
  Prism' Double Latitude
fracLatitude =
  prism' (\(Latitude d m s) ->
    fromIntegral (nDegreesLatitude # d) + nSeconds # s + fromIntegral (nMinutes # m * 60))
    (\x -> let (d, z) = properFraction x
               (m, s) = properFraction ((z :: Double) * 60)
           in do d' <- d ^? nDegreesLatitude
                 m' <- abs m ^? nMinutes
                 s' <- (abs s * 60) ^? nSeconds
                 return (Latitude d' m' s'))

class HasLatitude t where
  latitude ::
    Lens' t Latitude

instance HasLatitude Latitude where
  latitude =
    id

instance HasDegreesLatitude Latitude where
  degreesLatitude =
    lens (\(Latitude d _ _) -> d) (\(Latitude _ m s) d -> Latitude d m s)

instance HasMinutes Latitude where
  minutes =
    lens (\(Latitude _ m _) -> m) (\(Latitude d _ s) m -> Latitude d m s)

instance HasSeconds Latitude where
  seconds =
    lens (\(Latitude _ _ s) -> s) (\(Latitude d m _) s -> Latitude d m s)
