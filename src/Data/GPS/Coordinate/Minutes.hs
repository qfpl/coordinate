module Data.GPS.Coordinate.Minutes(
  Minutes
, minutes
) where

import Prelude(Int, Bool(..), Eq, Show, Ord(..), (&&))
import Data.Maybe(Maybe(..))
import Control.Lens(Prism', prism')

newtype Minutes =
  Minutes Int
  deriving (Eq, Show)

minutes ::
  Prism' Int Minutes
minutes =
  prism'
    (\(Minutes i) -> i)
    (\i -> case i >= 0 && i < 60 of
             True -> Just (Minutes i)
             False -> Nothing)
