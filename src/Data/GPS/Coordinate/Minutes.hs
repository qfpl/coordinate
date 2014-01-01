module Data.GPS.Coordinate.Minutes(
  Minutes
, HasMinutes(..)
, nMinutes
) where

import Prelude(Functor, Int, Bool(..), Eq, Show, Ord(..), (&&), id)
import Data.Maybe(Maybe(..))
import Control.Lens(Prism', Lens', prism')

newtype Minutes =
  Minutes Int
  deriving (Eq, Ord, Show)

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
