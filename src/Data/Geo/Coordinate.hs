{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Geo.Coordinate where

import Control.Category((.))
import Control.Applicative(Applicative((<*>), pure), liftA2, Alternative((<|>), empty))
import Control.Category(id)
import Control.Lens(makeClassy, Traversal',  Iso', iso, (^.), (&))
import Control.Monad(Monad((>>=), return))
import Control.Monad.Fix(MonadFix(mfix))
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Trans.Class(MonadTrans(lift))
import Control.Monad.Zip(MonadZip(mzip))
import Data.Eq(Eq)
import Data.Int(Int)
import Data.Functor(Functor(fmap), (<$>))
import Data.Ord(Ord)
import Prelude(Show, Double)

data ECEF =
  ECEF {
    _x ::
      Double
  , _y ::
      Double
  , _z ::
      Double
  } deriving (Eq, Ord, Show)

makeClassy ''ECEF

class HasDoubles a where
  doubles ::
    Traversal' a Double

instance HasDoubles Double where
  doubles =
    id

instance HasDoubles ECEF where
  doubles f (ECEF x_ y_ z_) =
    ECEF <$>
      f x_ <*>
      f y_ <*>
      f z_

data LLH =
  LLH {
    _lat ::
      Double
  , _lon ::
      Double
  , _height ::
      Double
  } deriving (Eq, Ord, Show)

makeClassy ''LLH

instance HasDoubles LLH where
  doubles f (LLH a o h) =
    LLH <$>
      f a <*>
      f o <*>
      f h

data Ellipsoid =
  Ellipsoid {
    _semiMajor ::
      Int
  , _flattening ::
      Double
  } deriving (Eq, Ord, Show)

makeClassy ''Ellipsoid

wgs84 ::
  Ellipsoid
wgs84 =
  Ellipsoid
    6378137
    298.257223563

newtype EllipsoidReader f a =
  EllipsoidReader (Ellipsoid -> f a)

runEllipsoidReader ::
  Iso'
    (EllipsoidReader f a)
    (Ellipsoid -> f a)
runEllipsoidReader =
  iso
    (\(EllipsoidReader k) -> k)
    EllipsoidReader

instance Functor f => Functor (EllipsoidReader f) where
  fmap f (EllipsoidReader k) =
    EllipsoidReader (fmap f . k)

instance Applicative f => Applicative (EllipsoidReader f) where
  pure =
    EllipsoidReader . pure . pure
  EllipsoidReader f <*> EllipsoidReader a =
      EllipsoidReader (liftA2 (<*>) f a)

instance Monad f => Monad (EllipsoidReader f) where
  return =
    EllipsoidReader . return . return
  EllipsoidReader k >>= f =
    EllipsoidReader (\e -> k e >>= \q -> e & f q ^. runEllipsoidReader)

instance Alternative f => Alternative (EllipsoidReader f) where
  empty =
    EllipsoidReader (pure empty)
  EllipsoidReader a <|> EllipsoidReader b =
    EllipsoidReader (liftA2 (<|>) a b)

instance MonadTrans EllipsoidReader where
  lift =
    EllipsoidReader . pure

instance MonadIO f => MonadIO (EllipsoidReader f) where
  liftIO =
    EllipsoidReader . pure . liftIO 

instance MonadFix f => MonadFix (EllipsoidReader f) where
  mfix f =
    EllipsoidReader (\e -> mfix (\q -> e & f q ^. runEllipsoidReader))

instance MonadZip f => MonadZip (EllipsoidReader f) where
  EllipsoidReader a `mzip` EllipsoidReader b =
    EllipsoidReader (liftA2 mzip a b)
