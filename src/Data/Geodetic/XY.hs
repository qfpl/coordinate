{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Geodetic.XY(
  XY(..)
, HasXY(..)
) where

import Data.Geodetic.HasDoubles(HasDoubles(doubles))
import Papa

data XY =
  XY {
    _x ::
      Double
  , _y ::
      Double
  }
  deriving (Eq, Ord, Show)

makeClassy ''XY

instance HasDoubles XY where
  doubles f (XY x_ y_) =
    XY <$>
      f x_ <*>
      f y_
