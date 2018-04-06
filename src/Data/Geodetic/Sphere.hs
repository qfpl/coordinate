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

newtype Sphere =
  Sphere Double
  deriving (Eq, Ord, Show)

makeWrapped ''Sphere

earthMean ::
  Sphere
earthMean =
  Sphere 6367450

ellipsoidSphere ::
  Prism'
    Ellipsoid
    Sphere
ellipsoidSphere = 
  prism'
    (\(Sphere s) -> Ellipsoid s 1)
    (\(Ellipsoid m f) -> if f == 1 then Just (Sphere m) else Nothing)
