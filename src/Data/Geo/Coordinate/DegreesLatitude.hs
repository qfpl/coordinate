{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Geo.Coordinate.DegreesLatitude(
  DegreesLatitude
, AsDegreesLatitude(..)
, modDegreesLatitude
) where

import Control.Applicative(Applicative)
import Control.Category(Category(id))
import Control.Lens(Optic', Choice, prism')
import Data.Bool((&&))
import Data.Eq(Eq((==)))
import Data.Int(Int)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord((<), (>)))
import Prelude(Show, mod, Num((+), (-)))

-- $setup
-- >>> import Control.Lens((#), (^?))
-- >>> import Data.Foldable(all)
-- >>> import Prelude(Eq(..))

newtype DegreesLatitude =
  DegreesLatitude Int
  deriving (Eq, Ord, Show)

class AsDegreesLatitude p f s where
  _DegreesLatitude ::
    Optic' p f s DegreesLatitude

instance AsDegreesLatitude p f DegreesLatitude where
  _DegreesLatitude =
    id

-- | A prism on degrees latitude to an integer between -90 and 90 exclusive.
--
-- >>> (7 :: Int) ^? _DegreesLatitude
-- Just (DegreesLatitude 7)
--
-- >>> (0 :: Int) ^? _DegreesLatitude
-- Just (DegreesLatitude 0)
--
-- >>> (89 :: Int) ^? _DegreesLatitude
-- Just (DegreesLatitude 89)
--
-- >>> (90 :: Int) ^? _DegreesLatitude
-- Nothing
--
-- >>> (-89 :: Int) ^? _DegreesLatitude
-- Just (DegreesLatitude (-89))
--
-- >>> (-90 :: Int) ^? _DegreesLatitude
-- Nothing
--
-- prop> all (\m -> _DegreesLatitude # m == (n :: Int)) (n ^? _DegreesLatitude)
instance (Choice p, Applicative f) => AsDegreesLatitude p f Int where
  _DegreesLatitude =
    prism'
      (\(DegreesLatitude i) -> i)
      (\i -> if i > -90 && i < 90
               then Just (DegreesLatitude i)
               else Nothing)

-- | Setting a value @>= 90@ will get that value @(`rem` 90)@.
--
-- >>> modDegreesLatitude 7
-- DegreesLatitude 7
--
-- >>> modDegreesLatitude 0
-- DegreesLatitude 0
--
-- >>> modDegreesLatitude 90
-- DegreesLatitude 90
--
-- >>> modDegreesLatitude (-90)
-- DegreesLatitude (-90)
--
-- >>> modDegreesLatitude 1
-- DegreesLatitude 1
--
-- >>> modDegreesLatitude 89
-- DegreesLatitude 89 
--
-- >>> modDegreesLatitude 91
-- DegreesLatitude (-89)
--
-- >>> modDegreesLatitude (-91)
-- DegreesLatitude 89
--
-- >>> modDegreesLatitude 300
-- DegreesLatitude (-60)
--
-- >>> modDegreesLatitude (-300)
-- DegreesLatitude 60
modDegreesLatitude ::
  Int
  -> DegreesLatitude
modDegreesLatitude x =
  DegreesLatitude (if x == 90 then 90 else mod (x + 90) 180 - 90)
