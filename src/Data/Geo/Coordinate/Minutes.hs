{-# LANGUAGE NoImplicitPrelude #-}

module Data.Geo.Coordinate.Minutes(
  Minutes
, HasMinutes(..)
, nMinutes
, modMinutes
) where

import Control.Category(Category(id))
import Control.Lens(Prism', Lens', prism')
import Data.Bool((&&))
import Data.Maybe(Maybe(Just, Nothing))
import Data.Eq(Eq)
import Data.Int(Int)
import Data.Fixed(mod')
import Data.Ord(Ord((>=), (<)))
import Prelude(Show)

-- $setup
-- >>> import Control.Lens((#), (^?))
-- >>> import Data.Foldable(all)
-- >>> import Prelude(Eq(..))

newtype Minutes =
  Minutes Int
  deriving (Eq, Ord, Show)

-- | Construct minutes such that if the given value is out of bounds,
-- a modulus is taken to keep it within 0 inclusive and 59 inclusive.
--
-- >>> modMinutes 7
-- Minutes 7
--
-- >>> modMinutes 0
-- Minutes 0
--
-- >>> modMinutes 60
-- Minutes 0
--
-- >>> modMinutes 61
-- Minutes 1
--
-- >>> modMinutes 59
-- Minutes 59
modMinutes ::
  Int
  -> Minutes
modMinutes x =
  Minutes (x `mod'` 60)

-- | A prism on minutes to an integer between 0 and 59 inclusive.
--
-- >>> 7 ^? nMinutes
-- Just (Minutes 7)
--
-- >>> 0 ^? nMinutes
-- Just (Minutes 0)
--
-- >>> 59 ^? nMinutes
-- Just (Minutes 59)
--
-- >>> 60 ^? nMinutes
-- Nothing
--
-- prop> all (\m -> nMinutes # m == n) (n ^? nMinutes)
nMinutes ::
  Prism' Int Minutes
nMinutes =
  prism'
    (\(Minutes i) -> i)
    (\i -> if i >= 0 && i < 60
             then Just (Minutes i)
             else Nothing)

class HasMinutes t where
  minutes ::
    Lens' t Minutes

instance HasMinutes Minutes where
  minutes =
    id
