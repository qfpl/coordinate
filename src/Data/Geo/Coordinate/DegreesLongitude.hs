{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Geo.Coordinate.DegreesLongitude(
  DegreesLongitude
, AsDegreesLongitude(..)
, modDegreesLongitude
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

newtype DegreesLongitude =
  DegreesLongitude Int
  deriving (Eq, Ord, Show)

class AsDegreesLongitude p f s where
  _DegreesLongitude ::
    Optic' p f s DegreesLongitude

instance AsDegreesLongitude p f DegreesLongitude where
  _DegreesLongitude =
    id

-- | A prism on degrees longitude to an integer between -180 and 180 exclusive.
--
-- >>> (7 :: Int) ^? _DegreesLongitude
-- Just (DegreesLongitude 7)
--
-- >>> (0 :: Int) ^? _DegreesLongitude
-- Just (DegreesLongitude 0)
--
-- >>> (179 :: Int) ^? _DegreesLongitude
-- Just (DegreesLongitude 179)
--
-- >>> (180 :: Int) ^? _DegreesLongitude
-- Nothing
--
-- >>> (-179 :: Int) ^? _DegreesLongitude
-- Just (DegreesLongitude (-179))
--
-- >>> (-180 :: Int) ^? _DegreesLongitude
-- Nothing
--
-- prop> all (\m -> _DegreesLongitude # m == (n :: Int)) (n ^? _DegreesLongitude)
instance (Choice p, Applicative f) => AsDegreesLongitude p f Int where
  _DegreesLongitude =
    prism'
      (\(DegreesLongitude i) -> i)
      (\i -> if i > -180 && i < 180
               then Just (DegreesLongitude i)
               else Nothing)

-- | Setting a value @>= 180@ will get that value @(`rem` 180)@.
--
-- >>> modDegreesLongitude 7
-- DegreesLongitude 7
--
-- >>> modDegreesLongitude 0
-- DegreesLongitude 0
--
-- >>> modDegreesLongitude 180
-- DegreesLongitude 180
--
-- >>> modDegreesLongitude (-180)
-- DegreesLongitude (-180)
--
-- >>> modDegreesLongitude 1
-- DegreesLongitude 1
--
-- >>> modDegreesLongitude 179
-- DegreesLongitude 179 
--
-- >>> modDegreesLongitude 181
-- DegreesLongitude (-179)
--
-- >>> modDegreesLongitude (-181)
-- DegreesLongitude 179
--
-- >>> modDegreesLongitude 600
-- DegreesLongitude (-120)
--
-- >>> modDegreesLongitude (-600)
-- DegreesLongitude 120
modDegreesLongitude ::
  Int
  -> DegreesLongitude
modDegreesLongitude x =
  DegreesLongitude (if x == 180 then 180 else mod (x + 180) 360 - 180)
