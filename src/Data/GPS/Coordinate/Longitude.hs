module Data.GPS.Coordinate.Longitude(
  Longitude
, longitude
, longitudeF
) where

import Prelude(Double, Eq, Show, Ord(..), Num(..), Bool(..), Monad(..), (&&), properFraction, fromIntegral)
import Control.Lens(Iso', Prism', iso, prism', (#), (^?))
import Data.GPS.Coordinate.DegreesLongitude
import Data.GPS.Coordinate.Minutes
import Data.GPS.Coordinate.Seconds

data Longitude =
  Longitude
    DegreesLongitude
    Minutes
    Seconds
  deriving (Eq, Show)

longitude ::
  Iso' (DegreesLongitude, Minutes, Seconds) Longitude
longitude =
  iso (\(d, m, s) -> Longitude d m s) (\(Longitude d m s) -> (d, m, s))

longitudeF ::
  Prism' Double Longitude
longitudeF =
  prism' (\(Longitude d m s) ->
    fromIntegral (degreesLongitude # d) + seconds # s + fromIntegral (minutes # m * 60))
    (\x -> let (d, z) = properFraction x
               (m, s) = properFraction ((z :: Double) * 60)
           in do d' <- d ^? degreesLongitude
                 m' <- m ^? minutes
                 s' <- s ^? seconds
                 return (Longitude d' m' s'))
