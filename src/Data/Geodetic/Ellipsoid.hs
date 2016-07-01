{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Geodetic.Ellipsoid(
  Ellipsoid(..)
, HasEllipsoid(..)
, flatteningReciprocal
, wgs84
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
