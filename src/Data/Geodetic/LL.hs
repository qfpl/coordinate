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

(<◦>) ::
  Double
  -> Double
  -> LL
t <◦> n =
  (t, n) ^. degrees
  