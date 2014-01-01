module Data.GPS.Coordinate.Seconds(
  Seconds
, HasSeconds(..)
, nSeconds
) where

import Prelude(Double, Bool(..), Eq, Show(..), Ord(..), id, (&&), (++), showParen, showString)
import Data.Maybe(Maybe(..))
import Control.Lens(Prism', Lens', prism')
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
    (\d -> case d >= 0 && d < 60 of
             True -> Just (Seconds d)
             False -> Nothing)

class HasSeconds t where
  seconds ::
    Lens' t Seconds

instance HasSeconds Seconds where
  seconds =
    id
