module Data.Geo.Coordinate.Longitude(
  Longitude
, HasLongitude(..)
, dmsLongitude
, fracLongitude
, radianLongitude
) where

import Prelude(Double, Eq, Show, Ord(..), Num(..), Floating(..), Fractional(..), Bool(..), Monad(..), id, (&&), (.), properFraction, fromIntegral)
import Control.Lens(Iso', Prism', Lens', iso, prism', lens, (#), (^?))
import Data.Geo.Coordinate.DegreesLongitude
import Data.Geo.Coordinate.Minutes
import Data.Geo.Coordinate.Seconds

-- $setup
-- >>> import Prelude(Functor(..))

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
--
-- >>> fmap (dmsLongitude #)  (7 ^? fracLongitude)
-- Just (DegreesLongitude 7,Minutes 0,Seconds 0.0000)
--
-- >>> fmap (dmsLongitude #)  (7.12 ^? fracLongitude)
-- Just (DegreesLongitude 7,Minutes 7,Seconds 12.0000)
dmsLongitude ::
  Iso' (DegreesLongitude, Minutes, Seconds) Longitude
dmsLongitude =
  iso (\(d, m, s) -> Longitude d m s) (\(Longitude d m s) -> (d, m, s))

-- | A prism on longitude to a double between -180 and 180 exclusive.
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
-- >>> fmap (fracLongitude #) (7.12 ^? fracLongitude)
-- Just 7.12
--
-- >>> fmap (fracLongitude #) ((-7.12) ^? fracLongitude)
-- Just (-7.12)
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
--
-- >>> fmap (fracLongitude #) (do deg <- 7 ^? nDegreesLongitude; min <- 7 ^? nMinutes; sec <- 7 ^? nSeconds; (deg, min, sec) ^? dmsLongitude)
-- Just 7.118611111111111
--
-- >>> fmap (fracLongitude #) (do deg <- 179 ^? nDegreesLongitude; min <- 15 ^? nMinutes; sec <- 6 ^? nSeconds; (deg, min, sec) ^? dmsLongitude)
-- Just 179.25166666666667
fracLongitude ::
  Prism' Double Longitude
fracLongitude =
  prism' (\(Longitude d m s) ->
    let p = fromIntegral (nDegreesLongitude # d)
        q = (fromIntegral (nMinutes # m) / 60) + (nSeconds # s) / 3600
    in (if p < 0 then (-) else (+)) p q)
    (\x -> let (d, z) = properFraction x
               (m, s) = properFraction ((z :: Double) * 60)
           in do d' <- d ^? nDegreesLongitude
                 m' <- abs m ^? nMinutes
                 s' <- (abs s * 60) ^? nSeconds
                 return (Longitude d' m' s'))

-- | A prism on longitude to a double between -π and π exclusive.
--
-- >>> 0.2 ^? radianLongitude
-- Just (Longitude (DegreesLongitude 11) (Minutes 27) (Seconds 32.9612))
--
-- >>> 1.3 ^? radianLongitude
-- Just (Longitude (DegreesLongitude 74) (Minutes 29) (Seconds 4.2481))
--
-- >>> (-1.3) ^? radianLongitude
-- Just (Longitude (DegreesLongitude (-74)) (Minutes 29) (Seconds 4.2481))
--
-- >>> 3.14159 ^? radianLongitude
-- Just (Longitude (DegreesLongitude 179) (Minutes 59) (Seconds 59.4527))
--
-- >>> 3.15 ^? radianLongitude
-- Nothing
--
-- >>> (-3.15) ^? radianLongitude
-- Nothing
--
-- >>> fmap (radianLongitude #) (do deg <- 7 ^? nDegreesLongitude; min <- 7 ^? nMinutes; sec <- 7 ^? nSeconds; (deg, min, sec) ^? dmsLongitude)
-- Just 0.12424320205794079
--
-- >>> fmap (radianLongitude #) (do deg <- 179 ^? nDegreesLongitude; min <- 15 ^? nMinutes; sec <- 6 ^? nSeconds; (deg, min, sec) ^? dmsLongitude)
-- Just 3.1285317730207023
radianLongitude ::
  Prism' Double Longitude
radianLongitude =
  iso (\n -> n * 180 / pi) (\n -> n * pi / 180) . fracLongitude

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
