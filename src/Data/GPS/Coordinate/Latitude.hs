module Data.GPS.Coordinate.Latitude(
  Latitude
, latitude
, latitudeF
) where

import Prelude(Double, Eq, Show, Ord(..), Num(..), Bool(..), Monad(..), (&&), properFraction, fromIntegral)
import Control.Lens(Iso', Prism', iso, prism', (#), (^?))
import Data.GPS.Coordinate.DegreesLatitude
import Data.GPS.Coordinate.Minutes
import Data.GPS.Coordinate.Seconds

data Latitude =
  Latitude
    DegreesLatitude
    Minutes
    Seconds
  deriving (Eq, Show)

latitude ::
  Iso' (DegreesLatitude, Minutes, Seconds) Latitude
latitude =
  iso (\(d, m, s) -> Latitude d m s) (\(Latitude d m s) -> (d, m, s))

latitudeF ::
  Prism' Double Latitude
latitudeF =
  prism' (\(Latitude d m s) ->
    fromIntegral (degreesLatitude # d) + seconds # s + fromIntegral (minutes # m * 60))
    (\x -> let (d, z) = properFraction x
               (m, s) = properFraction ((z :: Double) * 60)
           in do d' <- d ^? degreesLatitude
                 m' <- m ^? minutes
                 s' <- s ^? seconds
                 return (Latitude d' m' s'))
