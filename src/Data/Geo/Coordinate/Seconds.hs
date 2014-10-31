{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Geo.Coordinate.Seconds(
  Seconds
, AsSeconds(..)
, remSeconds
) where

import Control.Applicative(Applicative)
import Control.Category(Category(id))
import Control.Lens(prism', Optic', Choice)
import Data.Bool((&&))
import Data.Eq(Eq)
import Data.Int(Int)
import Data.Fixed(divMod')
import Data.Ord(Ord((>), (>=), (<)))
import Data.Maybe(Maybe(Just, Nothing))
import Data.List((++))
import Data.Tuple(snd)
import Prelude(Double, Show(showsPrec), showParen, showString)
import Text.Printf(printf)

-- $setup
-- >>> import Control.Lens((#), (^?), (^.))
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

class AsSeconds p f s where
  _Seconds ::
    Optic' p f s Seconds

instance AsSeconds p f Seconds where
  _Seconds =
    id

-- | A prism on seconds to a double between 0 inclusive and 60 exclusive.
--
-- >>> (7 :: Double) ^? _Seconds
-- Just (Seconds 7.0000)
--
-- >>> (0 :: Double) ^? _Seconds
-- Just (Seconds 0.0000)
--
-- >>> (59 :: Double) ^? _Seconds
-- Just (Seconds 59.0000)
--
-- >>> (59.99 :: Double) ^? _Seconds
-- Just (Seconds 59.9900)
--
-- >>> (60 :: Double) ^? _Seconds
-- Nothing
--
-- prop> all (\m -> _Seconds # m == (n :: Double)) (n ^? _Seconds)
instance (Choice p, Applicative f) => AsSeconds p f Double where
  _Seconds =
    prism'
      (\(Seconds d) -> d)
      (\d -> if d >= 0 && d < 60
               then Just (Seconds d)
               else Nothing)

-- | Setting a value @>= 60@ will get that value @(`rem` 60)@.
--
-- >>> remSeconds 7
-- Seconds 7.0000
--
-- >>> remSeconds 0
-- Seconds 0.0000
--
-- >>> remSeconds (-0.0001)
-- Seconds 59.9999
--
-- >>> remSeconds 60
-- Seconds 0.0000
--
-- >>> remSeconds 59.99999
-- Seconds 60.0000
--
-- >>> remSeconds 59.999
-- Seconds 59.9990
remSeconds ::
  Double
  -> Seconds
remSeconds x =
  Seconds (snd (x `divMod'` 60.0 :: (Int, Double)))
