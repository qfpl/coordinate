{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Geodetic.LL(
  LL(..)
, HasLL(..)
, degrees
, (<◦>)
) where

import Data.Geodetic.HasDoubles(HasDoubles(doubles))
import Papa

-- $setup
-- >>> import Control.Lens

data LL =
  LL {
    _lat ::
      Double -- radians
  , _lon ::
      Double -- radians
  }
  deriving (Eq, Ord, Show)

makeClassy ''LL

instance HasDoubles LL where
  doubles f (LL a o) =
    LL <$>
      f a <*>
      f o

-- |
--
-- >>> ((27.34, 152.15) ^. degrees)
-- LL {_lat = 0.47717301749524965, _lon = 2.6555184569093724}
--
-- >>> (degrees # LL 0.47717 2.65552)
-- (27.33982711025749,152.15008841258037)
degrees ::
  Iso'
    (Double, Double)
    LL
degrees =
  iso 
    (\(t, n) -> let r a = a / 180 * pi
                in  LL (r t) (r n))
    (\(LL t n) -> let r a = a / pi * 180
                  in  (r t, r n))

-- |
--
-- >>> 27.34 <◦> 152.15
-- LL {_lat = 0.47717301749524965, _lon = 2.6555184569093724}
--
-- >>> 61.94 <◦> (-152.15)
-- LL {_lat = 1.0810569386852877, _lon = -2.6555184569093724}
(<◦>) ::
  Double
  -> Double
  -> LL
t <◦> n =
  (t, n) ^. degrees
  