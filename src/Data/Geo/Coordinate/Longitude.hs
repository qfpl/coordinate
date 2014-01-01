module Data.Geo.Coordinate.Longitude(
  Longitude
, HasLongitude(..)
, dmsLongitude
, fracLongitude
) where

import Prelude(Double, Eq, Show, Ord(..), Num(..), Bool(..), Monad(..), id, (&&), properFraction, fromIntegral)
import Control.Lens(Iso', Prism', Lens', iso, prism', lens, (#), (^?))
import Data.Geo.Coordinate.DegreesLongitude
import Data.Geo.Coordinate.Minutes
import Data.Geo.Coordinate.Seconds

data Longitude =
  Longitude
    DegreesLongitude
    Minutes
    Seconds
  deriving (Eq, Ord, Show)

-- | An isomorphism on the triple of degrees longitude, minutes, seconds to a longitude.
--
-- >>> do deg <- 7 ^? nDegreesLongitude; min <- 7 ^? nMinutes; sec <- 7 ^? nSeconds; (deg, min, sec) ^? dmsLongitude
-- Just (Longitude (DegreesLongitude 7) (Minutes 7) (Seconds 7.0000))
--
-- >>> do deg <- 179 ^? nDegreesLongitude; min <- 59 ^? nMinutes; sec <- 59 ^? nSeconds; (deg, min, sec) ^? dmsLongitude
-- Just (Longitude (DegreesLongitude 179) (Minutes 59) (Seconds 59.0000))
--
-- >>> do deg <- (-7) ^? nDegreesLongitude; min <- 7 ^? nMinutes; sec <- 7 ^? nSeconds; (deg, min, sec) ^? dmsLongitude
-- Just (Longitude (DegreesLongitude (-7)) (Minutes 7) (Seconds 7.0000))
--
-- >>> do deg <- (-179) ^? nDegreesLongitude; min <- 59 ^? nMinutes; sec <- 59 ^? nSeconds; (deg, min, sec) ^? dmsLongitude
-- Just (Longitude (DegreesLongitude (-179)) (Minutes 59) (Seconds 59.0000))
--
-- >>> do deg <- 180 ^? nDegreesLongitude; min <- 59 ^? nMinutes; sec <- 59 ^? nSeconds; (deg, min, sec) ^? dmsLongitude
-- Nothing
--
-- >>> do deg <- 179 ^? nDegreesLongitude; min <- 60 ^? nMinutes; sec <- 59 ^? nSeconds; (deg, min, sec) ^? dmsLongitude
-- Nothing
--
-- >>> do deg <- 179 ^? nDegreesLongitude; min <- 59 ^? nMinutes; sec <- 60 ^? nSeconds; (deg, min, sec) ^? dmsLongitude
-- Nothing
dmsLongitude ::
  Iso' (DegreesLongitude, Minutes, Seconds) Longitude
dmsLongitude =
  iso (\(d, m, s) -> Longitude d m s) (\(Longitude d m s) -> (d, m, s))

-- | A prism on longitude to a double between -180 and 180 inclusive.
--
-- >>> 7 ^? fracLongitude
-- Just (Longitude (DegreesLongitude 7) (Minutes 0) (Seconds 0.0000))
--
-- >>> (-7) ^? fracLongitude
-- Just (Longitude (DegreesLongitude (-7)) (Minutes 0) (Seconds 0.0000))
--
-- >>> 7.12 ^? fracLongitude
-- Just (Longitude (DegreesLongitude 7) (Minutes 7) (Seconds 12.0000))
--
-- >>> (-7.12) ^? fracLongitude
-- Just (Longitude (DegreesLongitude (-7)) (Minutes 7) (Seconds 12.0000))
--
-- >>> 180 ^? fracLongitude
-- Nothing
--
-- >>> (-180) ^? fracLongitude
-- Nothing
--
-- >>> 15.63791 ^? fracLongitude
-- Just (Longitude (DegreesLongitude 15) (Minutes 38) (Seconds 16.4760))
--
-- >>> 179.1 ^? fracLongitude
-- Just (Longitude (DegreesLongitude 179) (Minutes 5) (Seconds 60.0000))
--
-- >>> 179.2 ^? fracLongitude
-- Just (Longitude (DegreesLongitude 179) (Minutes 11) (Seconds 60.0000))
fracLongitude ::
  Prism' Double Longitude
fracLongitude =
  prism' (\(Longitude d m s) ->
    fromIntegral (nDegreesLongitude  # d) + nSeconds # s + fromIntegral (nMinutes # m * 60))
    (\x -> let (d, z) = properFraction x
               (m, s) = properFraction ((z :: Double) * 60)
           in do d' <- d ^? nDegreesLongitude
                 m' <- abs m ^? nMinutes
                 s' <- (abs s * 60) ^? nSeconds
                 return (Longitude d' m' s'))

class HasLongitude t where
  longitude ::
    Lens' t Longitude

instance HasLongitude Longitude where
  longitude =
    id

instance HasDegreesLongitude Longitude where
  degreesLongitude =
    lens (\(Longitude d _ _) -> d) (\(Longitude _ m s) d -> Longitude d m s)

instance HasMinutes Longitude where
  minutes =
    lens (\(Longitude _ m _) -> m) (\(Longitude d _ s) m -> Longitude d m s)

instance HasSeconds Longitude where
  seconds =
    lens (\(Longitude _ _ s) -> s) (\(Longitude d m _) s -> Longitude d m s)
