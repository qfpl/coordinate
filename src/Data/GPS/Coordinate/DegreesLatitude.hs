module Data.GPS.Coordinate.DegreesLatitude(
  DegreesLatitude
, HasDegreesLatitude(..)
, nDegreesLatitude
) where

import Prelude(Int, Bool(..), Eq, Show, Ord(..), id, (&&))
import Data.Maybe(Maybe(..))
import Control.Lens(Prism', Lens', prism')

-- $setup
-- >>> import Control.Lens((#), (^?))
-- >>> import Data.Foldable(all)
-- >>> import Prelude(Eq(..))

newtype DegreesLatitude =
  DegreesLatitude Int
  deriving (Eq, Ord, Show)

-- | A prism on degrees latitude to an integer between -90 and 90 exclusive.
--
-- >>> 7 ^? nDegreesLatitude
-- Just (DegreesLatitude 7)
--
-- >>> 0 ^? nDegreesLatitude
-- Just (DegreesLatitude 0)
--
-- >>> 89 ^? nDegreesLatitude
-- Just (DegreesLatitude 89)
--
-- >>> 90 ^? nDegreesLatitude
-- Nothing
--
-- >>> (-89) ^? nDegreesLatitude
-- Just (DegreesLatitude (-89))
--
-- >>> (-90) ^? nDegreesLatitude
-- Nothing
--
-- prop> all (\m -> nDegreesLatitude # m == n) (n ^? nDegreesLatitude)
nDegreesLatitude ::
  Prism' Int DegreesLatitude
nDegreesLatitude =
  prism'
    (\(DegreesLatitude i) -> i)
    (\i -> case i > -90 && i < 90 of
             True -> Just (DegreesLatitude i)
             False -> Nothing)

class HasDegreesLatitude t where
  degreesLatitude ::
    Lens' t DegreesLatitude

instance HasDegreesLatitude DegreesLatitude where
  degreesLatitude =
    id

