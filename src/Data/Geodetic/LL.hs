{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Geodetic.LL(
  LL(..)
, HasLL(..)
) where

import Control.Applicative(Applicative((<*>)))
import Control.Lens(makeClassy)
import Data.Eq(Eq)
import Data.Functor((<$>))
import Data.Geodetic.HasDoubles(HasDoubles(doubles))
import Data.Ord(Ord)
import Prelude(Show, Double)

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
