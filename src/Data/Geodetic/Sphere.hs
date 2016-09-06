{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Geodetic.Sphere(
  Sphere(..)
, earthMean
, ellipsoidSphere
) where

import Data.Geodetic.Ellipsoid(Ellipsoid(Ellipsoid))
import Papa

-- $setup
-- >>> import Control.Lens

newtype Sphere =
  Sphere Double
  deriving (Eq, Ord, Show)

makeWrapped ''Sphere

-- |
--
-- >>> earthMean
-- Sphere 6367450.0
earthMean ::
  Sphere
earthMean =
  Sphere 6367450

-- |
--
-- >>> ellipsoidSphere # Sphere 77
-- Ellipsoid {_semiMajor = 77.0, _flattening = 1.0}
--
-- >>> Ellipsoid 77 1 ^? ellipsoidSphere
-- Just (Sphere 77.0)
--
-- >>> Ellipsoid 77 2 ^? ellipsoidSphere
-- Nothing
ellipsoidSphere ::
  Prism'
    Ellipsoid
    Sphere
ellipsoidSphere = 
  prism'
    (\(Sphere s) -> Ellipsoid s 1)
    (\(Ellipsoid m f) -> if f == 1 then Just (Sphere m) else Nothing)
