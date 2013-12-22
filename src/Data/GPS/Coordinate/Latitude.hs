module Data.GPS.Coordinate.Latitude(
  Latitude
, HasLatitude(..)
, dmsLatitude
, fracLatitude
) where

import Prelude(Double, Eq, Show, Ord(..), Num(..), Bool(..), Monad(..), id, (&&), properFraction, fromIntegral)
import Control.Lens(Iso', Prism', Lens', iso, prism', lens, (#), (^?))
import Data.GPS.Coordinate.DegreesLatitude
import Data.GPS.Coordinate.Minutes
import Data.GPS.Coordinate.Seconds

data Latitude =
  Latitude
    DegreesLatitude
    Minutes
    Seconds
  deriving (Eq, Show)

dmsLatitude ::
  Iso' (DegreesLatitude, Minutes, Seconds) Latitude
dmsLatitude =
  iso (\(d, m, s) -> Latitude d m s) (\(Latitude d m s) -> (d, m, s))

fracLatitude ::
  Prism' Double Latitude
fracLatitude =
  prism' (\(Latitude d m s) ->
    fromIntegral (nDegreesLatitude # d) + nSeconds # s + fromIntegral (nMinutes # m * 60))
    (\x -> let (d, z) = properFraction x
               (m, s) = properFraction ((z :: Double) * 60)
           in do d' <- d ^? nDegreesLatitude
                 m' <- m ^? nMinutes
                 s' <- s ^? nSeconds
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
