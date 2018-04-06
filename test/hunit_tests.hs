module Main (main) where

import Test.HUnit (Assertion, Test (TestCase, TestList), (@?=), runTestTT, failures, errors)

import Prelude
import Control.Lens ((^.), (^?), (#), ReifiedIso(runIso), from)
import Control.Monad (when)
import System.Exit (exitFailure)

import Data.Geodetic.ECEF
import Data.Geodetic.Ellipsoid
import Data.Geodetic.EllipsoidReaderT
import Data.Geodetic.LL
import Data.Geodetic.LLH
import Data.Geodetic.Sphere
import Data.Geodetic.XY

(@??=) :: Show a => a -> a -> Assertion
a @??= b = show a @?= show b
infix 1 @??=

assertions :: [Assertion]
assertions =
  [ wgs84' eccentricitySquared @??= 6.6943799901413165e-3
  , wgs84' eccentricitySquared' @??= 6.694455244784511e-3
  , wgs84' normal 7 @??= 6387371.845852088
  , wgs84' normal 71 @??= 6397535.266650572
  , wgs84' normal 711 @??= 6393308.675975408
  , wgs84' normal (-7) @??= 6387371.845852088
  , ECEF (XY (-5019624) 2618621) (-2927516) ^. runIso (wgs84' earthGeo) @?= LLH {ll = LL {_lat = -0.4799654447089294, _lon = 2.66075442877903}, _height = 100.20987554546446}
  , ECEF (XY 9919623 (-3116612)) (-2396517) ^. runIso (wgs84' earthGeo) @?= LLH {ll = LL {_lat = -0.22740831363634992, _lon = -0.30442061911398305}, _height = 4293252.6636643605}
  , LLH (LL 0.48 2.661) 100 ^. from (runIso (wgs84' earthGeo)) @?= ECEF {_xy = XY {_x = -5020176.908575072, _y = 2617341.3240995244}, _z = 2927710.5079646683}
  , LLH (LL (-0.22741) (-0.30442)) 4293252.66 ^. from (runIso (wgs84' earthGeo)) @?= ECEF {_xy = XY {_x = 9919621.069754401, _y = -3116604.645933256}, _z = -2396534.4668575544}
  , ((27.34, 152.15) ^. degrees) @?= LL {_lat = 0.47717301749524965, _lon = 2.6555184569093724}
  , (degrees # LL 0.47717 2.65552) @?= (27.33982711025749,152.15008841258037)
  , 27.34 <◦> 152.15 @?= LL {_lat = 0.47717301749524965, _lon = 2.6555184569093724}
  , 61.94 <◦> (-152.15) @?= LL {_lat = 1.0810569386852877, _lon = -2.6555184569093724}
  , earthMean @?= Sphere 6367450.0
  , ellipsoidSphere # Sphere 77 @?= Ellipsoid {_semiMajor = 77.0, _flattening = 1.0}
  , Ellipsoid 77 1 ^? ellipsoidSphere @?= Just (Sphere 77.0)
  , Ellipsoid 77 2 ^? ellipsoidSphere @?= Nothing
  ]

tests :: Test
tests =
  TestList $ fmap TestCase assertions

main :: IO ()
main = do
  c <- runTestTT tests
  when (errors c > 0 || failures c > 0) exitFailure
