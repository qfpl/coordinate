{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Geodetic.HasDoubles(
  HasDoubles(..)
) where

import Papa

class HasDoubles a where
  doubles ::
    Traversal' a Double

instance HasDoubles Double where
  doubles =
    id
