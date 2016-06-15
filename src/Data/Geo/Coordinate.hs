{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Geo.Coordinate where

import Control.Category((.))
import Control.Applicative(Applicative((<*>), pure), liftA2, Alternative((<|>), empty))
import Control.Category(id)
import Control.Lens(makeClassy, Traversal',  Iso', iso, from, (^.), (&), Wrapped(Unwrapped, _Wrapped'), Rewrapped)
import Control.Monad(Monad((>>=), return), MonadPlus(mzero, mplus))
import Control.Monad.Fix(MonadFix(mfix))
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Trans.Class(MonadTrans(lift))
import Control.Monad.Trans.Reader(ReaderT)
import Control.Monad.Zip(MonadZip(mzip))
import Data.Eq(Eq)
import Data.Functor(Functor(fmap), (<$>))
import Data.Functor.Identity(Identity(Identity, runIdentity))
import Data.Ord(Ord)
import Prelude(Show, Double, Num((*), (-)), Fractional((/)))

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
      Double
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

newtype EllipsoidReaderT f a =
  EllipsoidReaderT (Ellipsoid -> f a)

type EllipsoidReader a =
  EllipsoidReaderT Identity a

instance (t ~ EllipsoidReaderT f b) => Rewrapped (EllipsoidReaderT f a) t

instance Wrapped (EllipsoidReaderT f a) where
  type Unwrapped (EllipsoidReaderT f a) =
    Ellipsoid -> f a
  _Wrapped' =
    iso
      (\(EllipsoidReaderT k) -> k)
      EllipsoidReaderT

runEllipsoidReader ::
  Iso'
    (EllipsoidReader a)
    (Ellipsoid -> a)
runEllipsoidReader =
  iso
    (\(EllipsoidReaderT k) -> runIdentity . k)
    (\k -> EllipsoidReaderT (Identity . k))

runEllipsoidReaderT ::
  Iso'
    (EllipsoidReaderT f a)
    (ReaderT Ellipsoid f a)
runEllipsoidReaderT =
  from (_Wrapped' . from _Wrapped')

hoistEllipsoidReader ::
  Applicative f =>
  EllipsoidReader a
  -> EllipsoidReaderT f a
hoistEllipsoidReader (EllipsoidReaderT k) =
  EllipsoidReaderT (pure . runIdentity . k)

instance Functor f => Functor (EllipsoidReaderT f) where
  fmap f (EllipsoidReaderT k) =
    EllipsoidReaderT (fmap f . k)

instance Applicative f => Applicative (EllipsoidReaderT f) where
  pure =
    EllipsoidReaderT . pure . pure
  EllipsoidReaderT f <*> EllipsoidReaderT a =
      EllipsoidReaderT (liftA2 (<*>) f a)

instance Monad f => Monad (EllipsoidReaderT f) where
  return =
    EllipsoidReaderT . return . return
  EllipsoidReaderT k >>= f =
    EllipsoidReaderT (\e -> k e >>= \q -> e & f q ^. _Wrapped')

instance Alternative f => Alternative (EllipsoidReaderT f) where
  empty =
    EllipsoidReaderT (pure empty)
  EllipsoidReaderT a <|> EllipsoidReaderT b =
    EllipsoidReaderT (liftA2 (<|>) a b)

instance MonadPlus f => MonadPlus (EllipsoidReaderT f) where
  mzero =
    EllipsoidReaderT (pure mzero)
  EllipsoidReaderT a `mplus` EllipsoidReaderT b =
    EllipsoidReaderT (liftA2 mplus a b)

instance MonadTrans EllipsoidReaderT where
  lift =
    EllipsoidReaderT . pure

instance MonadIO f => MonadIO (EllipsoidReaderT f) where
  liftIO =
    EllipsoidReaderT . pure . liftIO 

instance MonadFix f => MonadFix (EllipsoidReaderT f) where
  mfix f =
    EllipsoidReaderT (\e -> mfix (\q -> e & f q ^. _Wrapped'))

instance MonadZip f => MonadZip (EllipsoidReaderT f) where
  EllipsoidReaderT a `mzip` EllipsoidReaderT b =
    EllipsoidReaderT (liftA2 mzip a b)

readEllipsoid ::
  Applicative f =>
  EllipsoidReaderT f Ellipsoid
readEllipsoid =
  EllipsoidReaderT pure

readSemiMajor ::
  Applicative f =>
  EllipsoidReaderT f Double
readSemiMajor =
  (^. semiMajor) <$> readEllipsoid

readFlattening ::
  Applicative f =>
  EllipsoidReaderT f Double
readFlattening =
  (^. flattening) <$> readEllipsoid

readSemiMinor ::
  Applicative f =>
  EllipsoidReaderT f Double
readSemiMinor =
  semiMinor <$> readEllipsoid

semiMinor ::
  HasEllipsoid c =>
  c
  -> Double
semiMinor e =
  let Ellipsoid m f = e ^. ellipsoid
  in m * (1 - 1 / f)
  