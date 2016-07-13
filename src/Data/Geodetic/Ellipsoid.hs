{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Geodetic.Ellipsoid(
  Ellipsoid(..)
, HasEllipsoid(..)
, flatteningReciprocal
-- * Ellipsoids
, wgs84
, grs80
, grs67
, ans
, wgs72
, au1965
, krasovsky1940
, international1924
, hayford1909
, airy1830
, everest1830
, bessel1841
, clarke1858
, clarke1866
, clarke1880
) where

import Control.Applicative(Applicative((<*>)))
import Control.Category((.))
import Control.Lens(makeClassy, Lens', involuted)
import Data.Eq(Eq)
import Data.Functor((<$>))
import Data.Geodetic.HasDoubles(HasDoubles(doubles))
import Data.Ord(Ord)
import Prelude(Show, Double, (/))

data Ellipsoid =
  Ellipsoid {
    _semiMajor ::
      Double
  , _flattening ::
      Double
  } deriving (Eq, Ord, Show)

makeClassy ''Ellipsoid

instance HasDoubles Ellipsoid where
  doubles f (Ellipsoid s l) =
    Ellipsoid <$> f s <*> f l

flatteningReciprocal ::
  HasEllipsoid e =>
  Lens'
    e
    Double
flatteningReciprocal =
  flattening . involuted (1/)

wgs84 ::
  Ellipsoid
wgs84 =
  Ellipsoid
    6378137
    298.257223563

grs80 ::
  Ellipsoid
grs80 =
  Ellipsoid
    6378137
    298.257222101

grs67 ::
  Ellipsoid
grs67 =
  Ellipsoid
    6378160
    298.25

ans ::
  Ellipsoid
ans =
  Ellipsoid
    6378160
    298.25

wgs72 ::
  Ellipsoid
wgs72 =
  Ellipsoid
    6378135
    298.26

au1965 ::
  Ellipsoid
au1965 =
  Ellipsoid
    6378160
    298.25

krasovsky1940 ::
  Ellipsoid
krasovsky1940 =
  Ellipsoid
    6378245
    298.3

international1924 ::
  Ellipsoid
international1924 =
  Ellipsoid
    6378388
    297

hayford1909 ::
  Ellipsoid
hayford1909 =
  international1924

airy1830 ::
  Ellipsoid
airy1830 =
  Ellipsoid
    6377563.4
    299.32

everest1830 ::
  Ellipsoid
everest1830 =
  Ellipsoid
    6377276.3
    300.8

bessel1841 ::
  Ellipsoid
bessel1841 =
  Ellipsoid
    6377397.2
    299.15

clarke1858 ::
  Ellipsoid
clarke1858 =
  Ellipsoid
    6378293.645
    294.26

clarke1866 ::
  Ellipsoid
clarke1866 =
  Ellipsoid
    6378206.4
    294.98

clarke1880 ::
  Ellipsoid
clarke1880 =
  Ellipsoid
    6378249.145
    293.465
    