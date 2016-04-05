{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Geodetic.XY where

import Control.Applicative(Applicative((<*>)))
import Control.Lens(makeClassy)
import Data.Eq(Eq)
import Data.Functor((<$>))
import Data.Geodetic.HasDoubles(HasDoubles(doubles))
import Data.Ord(Ord)
import Prelude(Show, Double)

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
