{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Geo.Coordinate where

import Control.Category((.))
import Control.Applicative(Applicative((<*>), pure), liftA2, Alternative((<|>), empty))
import Control.Category(id)
import Control.Lens(makeClassy, makeWrapped, involuted, Traversal', ReifiedIso', ReifiedIso(Iso), Iso', Lens', lens, iso, from, (^.), (&), Wrapped(_Wrapped'))
import Control.Monad(Monad((>>=), return), MonadPlus(mzero, mplus))
import Control.Monad.Fix(MonadFix(mfix))
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Trans.Class(MonadTrans(lift))
import Control.Monad.Trans.Reader(ReaderT)
import Control.Monad.Zip(MonadZip(mzip))
import Data.Eq(Eq)
import Data.Functor(Functor(fmap), (<$>))
import Data.Functor.Identity(Identity(Identity, runIdentity))
import Data.Int(Int)
import Data.Ord(Ord)
import Prelude(Show, Double, Num((*), (+), (-)), Fractional((/)), Floating(sin, cos, sqrt, atan, (**)), RealFloat(atan2), (^))

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

instance HasDoubles Ellipsoid where
  doubles f (Ellipsoid s l) =
    Ellipsoid <$> f s <*> f l

flatteningReciprocal ::
  HasEllipsoid e =>
  Lens'
    e
    Double
flatteningReciprocal =
  flattening . involuted (1/)

wgs84 ::
  Ellipsoid
wgs84 =
  Ellipsoid
    6378137
    298.257223563

newtype EllipsoidReaderT f a =
  EllipsoidReaderT (Ellipsoid -> f a)

makeWrapped ''EllipsoidReaderT

type EllipsoidReader a =
  EllipsoidReaderT Identity a

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

readFlatteningReciprocal ::
  Applicative f =>
  EllipsoidReaderT f Double
readFlatteningReciprocal =
  (^. flatteningReciprocal) <$> readEllipsoid

semiMinor ::
  Applicative f =>
  EllipsoidReaderT f Double
semiMinor =
  (\e -> let Ellipsoid m f = e ^. ellipsoid
         in m * (1 - 1 / f)) <$> readEllipsoid

eccentricitySquared ::
  Applicative f =>
  EllipsoidReaderT f Double
eccentricitySquared =
  (\f -> 2 * f - (f * f)) <$> readFlatteningReciprocal

eccentricitySquared' ::
  Applicative f => 
  EllipsoidReaderT f Double
eccentricitySquared' =
  (\f -> (f * (2 - f)) / (1 - f * f)) <$> readFlatteningReciprocal

distributeNormal ::
  Applicative f =>
  Double
  -> EllipsoidReaderT f Double
distributeNormal lat =
  (\k -> k lat) <$> normal

normal ::
  Applicative f =>
  EllipsoidReaderT f (Double -> Double)
normal =
  (\s e lat -> _semiMajor e / sqrt (1 - s * sin lat ^ (2 :: Int))) <$> eccentricitySquared <*> readEllipsoid


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
  let f e2 a nt =
        Iso (
          iso
            (\(ECEF (XY x_ y_) h_) -> 
              let sq q = q ^ (2 :: Int)
                  p2 = sq x_ + sq y_
                  a2 = sq a
                  e4 = sq e2
                  zeta = (1 - e2) * (sq h_ / a2)
                  rho = (p2 / a2 + zeta - e4) / 6
                  rho2 = sq rho
                  rho3 = rho * rho2
                  s = e4 * zeta * p2 / (4 * a2)
                  cbrt q = q ** (1 / 3)
                  t = cbrt (s + rho3 + sqrt (s * (s + 2 * rho3)))
                  u = rho + t + rho2 / t
                  v = sqrt (sq u + e4 * zeta)
                  w = e2 * (u + v - zeta) / (2 * v)
                  kappa = 1 + e2 * (sqrt (u + v + sq w) + w) / (u + v)
                  phi = atan (kappa * h_ / sqrt p2)
                  norm = nt phi
                  l = h_ + e2 * norm * sin phi
              in LLH (LL phi (atan2 y_ x_)) (sqrt (l ^ (2 :: Int) + p2) - norm))
            (\(LLH (LL t_ n_) h_) ->
              let n = nt t_
                  cs k = (n + h_) * cos t_ * k n_
                  z_ = (n * (1 - e2) + h_) * sin t_
              in ECEF (XY (cs cos) (cs sin)) z_)
        )
  in  f <$>
      eccentricitySquared <*>
      readSemiMajor <*>
      normal

{-
earthToGeo :: (GE.Ellipsoid e) => e -> GG.ECEF -> (Angle Double, Angle Double, Length Double)
earthToGeo e (x_,y_,z_) = (phi, ND.atan2 y_ x_, ND.sqrt (l ND.^ pos2 ND.+ p2) ND.- norm)
   where
      -- Naming: numeric suffix inicates power. Hence x2 = x * x, x3 = x2 * x, etc.
      p2 = x_ ND.^ pos2 ND.+ y_ ND.^ pos2
      a = GE.majorRadius e
      a2 = a ND.^ pos2
      e2 = GE.eccentricity2 e
      e4 = e2 ND.^ pos2
      zeta = (ND._1 ND.- e2) ND.* (z_ ND.^ pos2 ND./ a2)
      rho = (p2 ND./ a2 ND.+ zeta ND.- e4) ND./ ND._6
      rho2 = rho ND.^ pos2
      rho3 = rho ND.* rho2
      s = e4 ND.* zeta ND.* p2 ND./ (ND._4 ND.* a2)
      t = ND.cbrt (s ND.+ rho3 ND.+ ND.sqrt (s ND.* (s ND.+ ND._2 ND.* rho3)))
      u = rho ND.+ t ND.+ rho2 ND./ t
      v = ND.sqrt (u ND.^ pos2 ND.+ e4 ND.* zeta)
      w = e2 ND.* (u ND.+ v ND.- zeta) ND./ (ND._2 ND.* v)
      kappa = ND._1 ND.+ e2 ND.* (ND.sqrt (u ND.+ v ND.+ w ND.^ pos2) ND.+ w) ND./ (u ND.+ v)
      phi = ND.atan (kappa ND.* z_ ND./ ND.sqrt p2)
      norm = GE.normal e phi
      l = z_ ND.+ e2 ND.* norm ND.* ND.sin phi


--  -27.5 152.45 100
test1 :: LLH
test1 = (^. runIso ((earthGeo ^. runEllipsoidReader) wgs84)) (ECEF (XY (-5019624) 2618621) (-2927516))

test2 :: (Angle Double, Angle Double, Length Double)
test2 = earthToGeo GE.WGS84 ((-5019624) *~ metre, 2618621 *~ metre, (-2927516) *~ metre)
-- (-0.4799654447089294,2.66075442877903,100.20987554546446 m)

test3 :: ECEF
test3 = (^. from (runIso ((earthGeo ^. runEllipsoidReader) wgs84))) (LLH (LL 0.48 2.661) 100)

test4 :: GE.ECEF
test4 = GG.geoToEarth (GG.Geodetic (0.48 *~ radian) (2.661 *~ radian) (100 *~ metre) GE.WGS84)
-}

{-


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


lensReader ::
  Iso
    (ReifiedLens' a b)
    (ReifiedLens' a b)
    (ReaderT a (Store b) a)
    (ReaderT a (Store b) a)
lensReader =
  iso 
    (\(Lens l) -> ReaderT (\a -> store (\b -> set l b a) (a ^. l)))
    (\(ReaderT r) -> Lens (lens (pos . r) (\a b -> peek b (r a))))

posL ::
  Lens'
    (Store a b)
    a
posL =
  lens
    pos
    (\s -> store (`peek` s))

peekL ::
  Lens'
    (Store a b)
    (a -> b)
peekL =
  lens
    (\s -> (`peek` s))
    (\s a -> store a (pos s))

-}
