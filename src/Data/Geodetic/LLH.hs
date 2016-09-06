{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Geodetic.LLH(
  LLH(..)
, HasLLH(..)
) where

import Data.Geodetic.HasDoubles(HasDoubles(doubles))
import Data.Geodetic.LL(HasLL(lL), LL)
import Papa

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
