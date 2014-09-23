{-# LANGUAGE NoImplicitPrelude #-}

module Data.Geo.Coordinate.DegreesLongitude(
  DegreesLongitude
, HasDegreesLongitude(..)
, nDegreesLongitude
) where

import Control.Category(Category(id))
import Control.Lens(Prism', Lens', prism')
import Data.Bool(Bool(True, False), (&&))
import Data.Eq(Eq)
import Data.Int(Int)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord((<), (>)))
import Prelude(Show)

-- $setup
-- >>> import Control.Lens((#), (^?))
-- >>> import Data.Foldable(all)
-- >>> import Prelude(Eq(..))

newtype DegreesLongitude =
  DegreesLongitude Int
  deriving (Eq, Ord, Show)

-- | A prism on degrees longitude to an integer between -180 and 180 exclusive.
--
-- >>> 7 ^? nDegreesLongitude
-- Just (DegreesLongitude 7)
--
-- >>> 0 ^? nDegreesLongitude
-- Just (DegreesLongitude 0)
--
-- >>> 179 ^? nDegreesLongitude
-- Just (DegreesLongitude 179)
--
-- >>> 180 ^? nDegreesLongitude
-- Nothing
--
-- >>> (-179) ^? nDegreesLongitude
-- Just (DegreesLongitude (-179))
--
-- >>> (-180) ^? nDegreesLongitude
-- Nothing
--
-- prop> all (\m -> nDegreesLongitude # m == n) (n ^? nDegreesLongitude)
nDegreesLongitude ::
  Prism' Int DegreesLongitude
nDegreesLongitude  =
  prism'
    (\(DegreesLongitude i) -> i)
    (\i -> case i > -180 && i < 180 of
             True -> Just (DegreesLongitude i)
             False -> Nothing)

class HasDegreesLongitude t where
  degreesLongitude ::
    Lens' t DegreesLongitude

instance HasDegreesLongitude DegreesLongitude where
  degreesLongitude =
    id
