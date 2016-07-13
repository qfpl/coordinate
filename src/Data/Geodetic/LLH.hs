{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Geodetic.LLH(
  LLH(..)
, HasLLH(..)
) where

import Control.Applicative(Applicative((<*>)))
import Control.Lens(makeClassy, lens)
import Data.Eq(Eq)
import Data.Functor((<$>))
import Data.Geodetic.HasDoubles(HasDoubles(doubles))
import Data.Geodetic.LL
import Data.Ord(Ord)
import Prelude(Show, Double)

data LLH =
  LLH {
    ll ::
      LL
  , _height ::
      Double
  } deriving (Eq, Ord, Show)

makeClassy ''LLH

instance HasLL LLH where
  lL =
    lens
      ll
      (\(LLH _ h) l -> LLH l h)

instance HasDoubles LLH where
  doubles f (LLH l h) =
    LLH <$>
      doubles f l <*>
      f h
