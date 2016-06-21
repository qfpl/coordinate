{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Geo.Coordinate where

import Control.Category((.))
import Control.Applicative(Applicative((<*>), pure), liftA2, Alternative((<|>), empty))
import Control.Category(id)
import Control.Lens(makeClassy, Traversal', ReifiedIso', ReifiedIso(Iso), Iso', Lens', lens, iso, from, (^.), (&), Wrapped(Unwrapped, _Wrapped'), Rewrapped)
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
import Prelude(Show, Double, Num((*), (+), (-)), Fractional((/)))

class HasDoubles a where
  doubles ::
    Traversal' a Double

instance HasDoubles Double where
  doubles =
    id

data XY =
  XY {
    _x ::
      Double
  , _y ::
      Double
  }
  deriving (Eq, Ord, Show)

makeClassy ''XY

instance HasDoubles XY where
  doubles f (XY x_ y_) =
    XY <$>
      f x_ <*>
      f y_

data ECEF =
  ECEF {
    _xy ::
    XY
  , _z ::
      Double
  }
  deriving (Eq, Ord, Show)

makeClassy ''ECEF

instance HasXY ECEF where
  xY =
    lens
      (\(ECEF xy_ _) -> xy_)
      (\(ECEF _ z_) xy_ -> ECEF xy_ z_)

instance HasDoubles ECEF where
  doubles f (ECEF xy_ z_) =
    ECEF <$>
      doubles f xy_ <*>
      f z_

----

data Ellipsoid =
  Ellipsoid {
    _semiMajor ::
      Double
  , _flattening ::
      Double
  } deriving (Eq, Ord, Show)

makeClassy ''Ellipsoid
  
flatteningPreservingSemiMinor ::
  Lens'
    Ellipsoid
    Double
flatteningPreservingSemiMinor =
  lens
    semiMinor
    (\(Ellipsoid _ f) n -> Ellipsoid (n / (1 - 1 / f)) f)

semiMajorPreservingSemiMinor ::
  Lens'
    Ellipsoid
    Double
semiMajorPreservingSemiMinor =
  lens
    semiMinor
    (\(Ellipsoid m _) n -> Ellipsoid m (1 / (n / m + 1)))

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

arrEllipsoidReader ::
  Applicative f =>
  (Ellipsoid -> a)
  -> EllipsoidReaderT f a
arrEllipsoidReader k =
  EllipsoidReaderT (pure . k)

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
    EllipsoidReaderT (\_ -> empty)
  EllipsoidReaderT a <|> EllipsoidReaderT b =
    EllipsoidReaderT (liftA2 (<|>) a b)

instance MonadPlus f => MonadPlus (EllipsoidReaderT f) where
  mzero =
    EllipsoidReaderT (\_ -> mzero)
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

eccentricitySquared ::
  Applicative f =>
  EllipsoidReaderT f Double
eccentricitySquared =
  (\f -> 2 * f - (f * f)) <$> readFlattening

eccentricitySquared' ::
  Applicative f =>
  EllipsoidReaderT f Double
eccentricitySquared' =
  (\f -> (f * (2 - f)) / (1 - f * f)) <$> readFlattening

----

data LL =
  LL {
    _lat ::
      Double -- radians
  , _lon ::
      Double -- radians
  }
  deriving (Eq, Ord, Show)

makeClassy ''LL

instance HasDoubles LL where
  doubles f (LL a o) =
    LL <$>
      f a <*>
      f o

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

----

earthGeo ::
  Applicative f =>
  EllipsoidReaderT f (ReifiedIso' ECEF LLH)
earthGeo =
  arrEllipsoidReader (\e -> Iso (
    iso
      (\(ECEF (XY x_ y_) h_) -> undefined)
      (\(LLH (LL t_ n_) h_) -> undefined)
  ))

undefined = undefined

{-


> earthToGeo WGS84 (999 *~ metre, 88888 *~ metre, 7777 *~ metre)
(0.16477260829264312,1.5595579375715796,-6288597.323607219 m)

> earthToGeo WGS84 ((-5014967.33) *~ metre, 2614625.24 *~ metre, 2938968.021 *~ metre)
(0.4820000002381146,2.6609999992939435,99.99797226581722 m)

> geoToEarth (Geodetic (27.65 *~ degree) (152.45 *~ degree) (100 *~ metre) WGS84)
(-5012801.572508958 m,2615061.7970748367 m,2942250.433250758 m)

> geoToEarth (Geodetic (0.482 *~ radian) (2.661 *~ radian) (100 *~ metre) WGS84)
(-5014967.3340606885 m,2614625.2376137706 m,2938968.02060036 m)


geoToEarth :: (Ellipsoid e) => Geodetic e -> ECEF
geoToEarth geo = (
      (n + h) * coslat * coslong,
      (n + h) * coslat * sinlong,
      (n * (_1 - eccentricity2 e) + h) * sinlat)
   where 
      n = normal e $ latitude geo
      e = ellipsoid geo
      coslat = cos $ latitude geo
      coslong = cos $ longitude geo
      sinlat = sin $ latitude geo
      sinlong = sin $ longitude geo
      h = altitude geo

normal :: (Ellipsoid e) => e -> Angle Double -> Length Double
normal e lat = majorRadius e / sqrt (_1 - eccentricity2 e * sin lat ^ pos2)

eccentricity2 :: (Ellipsoid e) => e -> Dimensionless Double
eccentricity2 e = _2 * f - (f * f) where f = flattening e


earthToGeo :: (Ellipsoid e) => e -> ECEF -> (Angle Double, Angle Double, Length Double)
earthToGeo e (x,y,z) = (phi, atan2 y x, sqrt (l ^ pos2 + p2) - norm)
   where
      -- Naming: numeric suffix inicates power. Hence x2 = x * x, x3 = x2 * x, etc.
      p2 = x ^ pos2 + y ^ pos2
      a = majorRadius e
      a2 = a ^ pos2
      e2 = eccentricity2 e
      e4 = e2 ^ pos2
      zeta = (_1-e2) * (z ^ pos2 / a2)
      rho = (p2 / a2 + zeta - e4) / _6
      rho2 = rho ^ pos2
      rho3 = rho * rho2
      s = e4 * zeta * p2 / (_4 * a2)
      t = cbrt (s + rho3 + sqrt (s * (s + _2 * rho3)))
      u = rho + t + rho2 / t
      v = sqrt (u ^ pos2 + e4 * zeta)
      w = e2 * (u + v - zeta) / (_2 * v)
      kappa = _1 + e2 * (sqrt (u + v + w ^ pos2) + w) / (u + v)
      phi = atan (kappa * z / sqrt p2)
      norm = normal e phi
      l = z + e2 * norm * sin phi

-}
