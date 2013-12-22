module Data.GPS.Coordinate.Longitude(
  Longitude
, HasLongitude(..)
, dmsLongitude
, fracLongitude
) where

import Prelude(Double, Eq, Show, Ord(..), Num(..), Bool(..), Monad(..), id, (&&), properFraction, fromIntegral)
import Control.Lens(Iso', Prism', Lens', iso, prism', lens, (#), (^?))
import Data.GPS.Coordinate.DegreesLongitude
import Data.GPS.Coordinate.Minutes
import Data.GPS.Coordinate.Seconds

data Longitude =
  Longitude
    DegreesLongitude
    Minutes
    Seconds
  deriving (Eq, Show)

dmsLongitude ::
  Iso' (DegreesLongitude, Minutes, Seconds) Longitude
dmsLongitude =
  iso (\(d, m, s) -> Longitude d m s) (\(Longitude d m s) -> (d, m, s))

fracLongitude ::
  Prism' Double Longitude
fracLongitude =
  prism' (\(Longitude d m s) ->
    fromIntegral (nDegreesLongitude  # d) + nSeconds # s + fromIntegral (nMinutes # m * 60))
    (\x -> let (d, z) = properFraction x
               (m, s) = properFraction ((z :: Double) * 60)
           in do d' <- d ^? nDegreesLongitude
                 m' <- m ^? nMinutes
                 s' <- s ^? nSeconds
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
