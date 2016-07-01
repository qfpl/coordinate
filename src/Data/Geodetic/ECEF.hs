{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Geodetic.ECEF(
  ECEF(..)
, HasECEF(..)
) where

import Control.Applicative(Applicative((<*>)))
import Control.Lens(makeClassy, lens)
import Data.Eq(Eq)
import Data.Functor((<$>))
import Data.Geodetic.HasDoubles(HasDoubles(doubles))
import Data.Geodetic.XY(XY, HasXY(xY))
import Data.Ord(Ord)
import Prelude(Show, Double)

data ECEF =
  ECEF {
    _xy ::
    XY
  , _z ::
      Double
  }
  deriving (Eq, Ord, Show)

makeClassy ''ECEF

instance HasXY ECEF where
  xY =
    lens
      (\(ECEF xy_ _) -> xy_)
      (\(ECEF _ z_) xy_ -> ECEF xy_ z_)

instance HasDoubles ECEF where
  doubles f (ECEF xy_ z_) =
    ECEF <$>
      doubles f xy_ <*>
      f z_
