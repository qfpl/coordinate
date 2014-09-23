{-# LANGUAGE NoImplicitPrelude #-}

module Data.Geo.Coordinate.Seconds(
  Seconds
, HasSeconds(..)
, modSeconds
, nSeconds
) where

import Control.Category(Category(id))
import Control.Lens(Prism', Lens', prism')
import Data.Bool((&&))
import Data.Eq(Eq)
import Data.Fixed(mod')
import Data.Ord(Ord((>), (>=), (<)))
import Data.Maybe(Maybe(Just, Nothing))
import Data.List((++))
import Prelude(Double, Show(showsPrec), showParen, showString)
import Text.Printf(printf)

-- $setup
-- >>> import Control.Lens((#), (^?))
-- >>> import Data.Foldable(all)
-- >>> import Prelude(Eq(..))

newtype Seconds =
  Seconds Double
  deriving (Eq, Ord)

-- | A show instance that prints to 4 decimal places.
-- This is to take floating-point rounding errors into account.
instance Show Seconds where
  showsPrec n (Seconds d) =
    showParen (n > 10) (showString ("Seconds " ++ printf "%0.4f" d))

-- | Construct seconds such that if the given value is out of bounds,
-- a modulus is taken to keep it within 0 inclusive and 60 exclusive.
--
-- >>> modSeconds 7
-- Seconds 7.0000
--
-- >>> modSeconds 0
-- Seconds 0.0000
--
-- >>> modSeconds (-0.0001)
-- Seconds 59.9999
--
-- >>> modSeconds 60
-- Seconds 0.0000
--
-- >>> modSeconds 59.99999
-- Seconds 60.0000
--
-- >>> modSeconds 59.999
-- Seconds 59.9990
modSeconds ::
  Double
  -> Seconds
modSeconds x =
  Seconds (x `mod'` 60)

-- | A prism on seconds to a double between 0 inclusive and 60 exclusive.
--
-- >>> 7 ^? nSeconds
-- Just (Seconds 7.0000)
--
-- >>> 0 ^? nSeconds
-- Just (Seconds 0.0000)
--
-- >>> 59 ^? nSeconds
-- Just (Seconds 59.0000)
--
-- >>> 59.99 ^? nSeconds
-- Just (Seconds 59.9900)
--
-- >>> 60 ^? nSeconds
-- Nothing
--
-- prop> all (\m -> nSeconds # m == n) (n ^? nSeconds)
nSeconds ::
  Prism' Double Seconds
nSeconds =
  prism'
    (\(Seconds d) -> d)
    (\d -> if d >= 0 && d < 60
             then Just (Seconds d)
             else Nothing)

class HasSeconds t where
  seconds ::
    Lens' t Seconds

instance HasSeconds Seconds where
  seconds =
    id
