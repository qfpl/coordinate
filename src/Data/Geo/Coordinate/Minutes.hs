module Data.Geo.Coordinate.Minutes(
  Minutes
, HasMinutes(..)
, nMinutes
) where

import Prelude(Functor, Int, Bool(..), Eq, Show, Ord(..), (&&), id)
import Data.Maybe(Maybe(..))
import Control.Lens(Prism', Lens', prism')

-- $setup
-- >>> import Control.Lens((#), (^?))
-- >>> import Data.Foldable(all)
-- >>> import Prelude(Eq(..))

newtype Minutes =
  Minutes Int
  deriving (Eq, Ord, Show)

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
    (\i -> case i >= 0 && i < 60 of
             True -> Just (Minutes i)
             False -> Nothing)

class HasMinutes t where
  minutes ::
    Lens' t Minutes

instance HasMinutes Minutes where
  minutes =
    id
