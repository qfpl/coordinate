{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Geo.Coordinate ({- module C, -} IntegralLatitude01_89(..), IntegralLatitude(..), IntegralLongitude00x_17x(..), IntegralLongitude000_179(..), IntegralLongitude(..)) where

import Prelude
import Data.Bool
import Data.Digit
import Data.Foldable
import Data.Maybe
import Control.Lens

{-
import Data.Geo.Coordinate.Coordinate as C
import Data.Geo.Coordinate.DegreesLatitude as C
import Data.Geo.Coordinate.DegreesLongitude as C
import Data.Geo.Coordinate.Latitude as C
import Data.Geo.Coordinate.Longitude as C
import Data.Geo.Coordinate.Minutes as C
import Data.Geo.Coordinate.Seconds as C
-}

data IntegralLatitude01_89 =
  IntegralLatitude_01
  | IntegralLatitude_02
  | IntegralLatitude_03
  | IntegralLatitude_04
  | IntegralLatitude_05
  | IntegralLatitude_06
  | IntegralLatitude_07
  | IntegralLatitude_08
  | IntegralLatitude_09
  | IntegralLatitude_10
  | IntegralLatitude_11
  | IntegralLatitude_12
  | IntegralLatitude_13
  | IntegralLatitude_14
  | IntegralLatitude_15
  | IntegralLatitude_16
  | IntegralLatitude_17
  | IntegralLatitude_18
  | IntegralLatitude_19
  | IntegralLatitude_20
  | IntegralLatitude_21
  | IntegralLatitude_22
  | IntegralLatitude_23
  | IntegralLatitude_24
  | IntegralLatitude_25
  | IntegralLatitude_26
  | IntegralLatitude_27
  | IntegralLatitude_28
  | IntegralLatitude_29
  | IntegralLatitude_30
  | IntegralLatitude_31
  | IntegralLatitude_32
  | IntegralLatitude_33
  | IntegralLatitude_34
  | IntegralLatitude_35
  | IntegralLatitude_36
  | IntegralLatitude_37
  | IntegralLatitude_38
  | IntegralLatitude_39
  | IntegralLatitude_40
  | IntegralLatitude_41
  | IntegralLatitude_42
  | IntegralLatitude_43
  | IntegralLatitude_44
  | IntegralLatitude_45
  | IntegralLatitude_46
  | IntegralLatitude_47
  | IntegralLatitude_48
  | IntegralLatitude_49
  | IntegralLatitude_50
  | IntegralLatitude_51
  | IntegralLatitude_52
  | IntegralLatitude_53
  | IntegralLatitude_54
  | IntegralLatitude_55
  | IntegralLatitude_56
  | IntegralLatitude_57
  | IntegralLatitude_58
  | IntegralLatitude_59
  | IntegralLatitude_60
  | IntegralLatitude_61
  | IntegralLatitude_62
  | IntegralLatitude_63
  | IntegralLatitude_64
  | IntegralLatitude_65
  | IntegralLatitude_66
  | IntegralLatitude_67
  | IntegralLatitude_68
  | IntegralLatitude_69
  | IntegralLatitude_70
  | IntegralLatitude_71
  | IntegralLatitude_72
  | IntegralLatitude_73
  | IntegralLatitude_74
  | IntegralLatitude_75
  | IntegralLatitude_76
  | IntegralLatitude_77
  | IntegralLatitude_78
  | IntegralLatitude_79
  | IntegralLatitude_80
  | IntegralLatitude_81
  | IntegralLatitude_82
  | IntegralLatitude_83
  | IntegralLatitude_84
  | IntegralLatitude_85
  | IntegralLatitude_86
  | IntegralLatitude_87
  | IntegralLatitude_88
  | IntegralLatitude_89
  deriving (Eq, Ord, Show)

integralLatitude01_89 ::
  Integral a =>
  Prism' a IntegralLatitude01_89
integralLatitude01_89 =
  prism'
    (\l -> case l of
             IntegralLatitude_01 ->
               1
             IntegralLatitude_02 ->
               2
             IntegralLatitude_03 ->
               3
             IntegralLatitude_04 ->
               4
             IntegralLatitude_05 ->
               5
             IntegralLatitude_06 ->
               6
             IntegralLatitude_07 ->
               7
             IntegralLatitude_08 ->
               8
             IntegralLatitude_09 ->
               9
             IntegralLatitude_10 ->
               10
             IntegralLatitude_11 ->
               11
             IntegralLatitude_12 ->
               12
             IntegralLatitude_13 ->
               13
             IntegralLatitude_14 ->
               14
             IntegralLatitude_15 ->
               15
             IntegralLatitude_16 ->
               16
             IntegralLatitude_17 ->
               17
             IntegralLatitude_18 ->
               18
             IntegralLatitude_19 ->
               19
             IntegralLatitude_20 ->
               20
             IntegralLatitude_21 ->
               21
             IntegralLatitude_22 ->
               22
             IntegralLatitude_23 ->
               23
             IntegralLatitude_24 ->
               24
             IntegralLatitude_25 ->
               25
             IntegralLatitude_26 ->
               26
             IntegralLatitude_27 ->
               27
             IntegralLatitude_28 ->
               28
             IntegralLatitude_29 ->
               29
             IntegralLatitude_30 ->
               30
             IntegralLatitude_31 ->
               31
             IntegralLatitude_32 ->
               32
             IntegralLatitude_33 ->
               33
             IntegralLatitude_34 ->
               34
             IntegralLatitude_35 ->
               35
             IntegralLatitude_36 ->
               36
             IntegralLatitude_37 ->
               37
             IntegralLatitude_38 ->
               38
             IntegralLatitude_39 ->
               39
             IntegralLatitude_40 ->
               40
             IntegralLatitude_41 ->
               41
             IntegralLatitude_42 ->
               42
             IntegralLatitude_43 ->
               43
             IntegralLatitude_44 ->
               44
             IntegralLatitude_45 ->
               45
             IntegralLatitude_46 ->
               46
             IntegralLatitude_47 ->
               47
             IntegralLatitude_48 ->
               48
             IntegralLatitude_49 ->
               49
             IntegralLatitude_50 ->
               50
             IntegralLatitude_51 ->
               51
             IntegralLatitude_52 ->
               52
             IntegralLatitude_53 ->
               53
             IntegralLatitude_54 ->
               54
             IntegralLatitude_55 ->
               55
             IntegralLatitude_56 ->
               56
             IntegralLatitude_57 ->
               57
             IntegralLatitude_58 ->
               58
             IntegralLatitude_59 ->
               59
             IntegralLatitude_60 ->
               60
             IntegralLatitude_61 ->
               61
             IntegralLatitude_62 ->
               62
             IntegralLatitude_63 ->
               63
             IntegralLatitude_64 ->
               64
             IntegralLatitude_65 ->
               65
             IntegralLatitude_66 ->
               66
             IntegralLatitude_67 ->
               67
             IntegralLatitude_68 ->
               68
             IntegralLatitude_69 ->
               69
             IntegralLatitude_70 ->
               70
             IntegralLatitude_71 ->
               71
             IntegralLatitude_72 ->
               72
             IntegralLatitude_73 ->
               73
             IntegralLatitude_74 ->
               74
             IntegralLatitude_75 ->
               75
             IntegralLatitude_76 ->
               76
             IntegralLatitude_77 ->
               77
             IntegralLatitude_78 ->
               78
             IntegralLatitude_79 ->
               79
             IntegralLatitude_80 ->
               80
             IntegralLatitude_81 ->
               81
             IntegralLatitude_82 ->
               82
             IntegralLatitude_83 ->
               83
             IntegralLatitude_84 ->
               84
             IntegralLatitude_85 ->
               85
             IntegralLatitude_86 ->
               86
             IntegralLatitude_87 ->
               87
             IntegralLatitude_88 ->
               88
             IntegralLatitude_89 ->
               89)
    (\l -> case l of 
             1 ->
               Just IntegralLatitude_01
             2 ->
               Just IntegralLatitude_02
             3 ->
               Just IntegralLatitude_03
             4 ->
               Just IntegralLatitude_04
             5 ->
               Just IntegralLatitude_05
             6 ->
               Just IntegralLatitude_06
             7 ->
               Just IntegralLatitude_07
             8 ->
               Just IntegralLatitude_08
             9 ->
               Just IntegralLatitude_09
             10 ->
               Just IntegralLatitude_10
             11 ->
               Just IntegralLatitude_11
             12 ->
               Just IntegralLatitude_12
             13 ->
               Just IntegralLatitude_13
             14 ->
               Just IntegralLatitude_14
             15 ->
               Just IntegralLatitude_15
             16 ->
               Just IntegralLatitude_16
             17 ->
               Just IntegralLatitude_17
             18 ->
               Just IntegralLatitude_18
             19 ->
               Just IntegralLatitude_19
             20 ->
               Just IntegralLatitude_20
             21 ->
               Just IntegralLatitude_21
             22 ->
               Just IntegralLatitude_22
             23 ->
               Just IntegralLatitude_23
             24 ->
               Just IntegralLatitude_24
             25 ->
               Just IntegralLatitude_25
             26 ->
               Just IntegralLatitude_26
             27 ->
               Just IntegralLatitude_27
             28 ->
               Just IntegralLatitude_28
             29 ->
               Just IntegralLatitude_29
             30 ->
               Just IntegralLatitude_30
             31 ->
               Just IntegralLatitude_31
             32 ->
               Just IntegralLatitude_32
             33 ->
               Just IntegralLatitude_33
             34 ->
               Just IntegralLatitude_34
             35 ->
               Just IntegralLatitude_35
             36 ->
               Just IntegralLatitude_36
             37 ->
               Just IntegralLatitude_37
             38 ->
               Just IntegralLatitude_38
             39 ->
               Just IntegralLatitude_39
             40 ->
               Just IntegralLatitude_40
             41 ->
               Just IntegralLatitude_41
             42 ->
               Just IntegralLatitude_42
             43 ->
               Just IntegralLatitude_43
             44 ->
               Just IntegralLatitude_44
             45 ->
               Just IntegralLatitude_45
             46 ->
               Just IntegralLatitude_46
             47 ->
               Just IntegralLatitude_47
             48 ->
               Just IntegralLatitude_48
             49 ->
               Just IntegralLatitude_49
             50 ->
               Just IntegralLatitude_50
             51 ->
               Just IntegralLatitude_51
             52 ->
               Just IntegralLatitude_52
             53 ->
               Just IntegralLatitude_53
             54 ->
               Just IntegralLatitude_54
             55 ->
               Just IntegralLatitude_55
             56 ->
               Just IntegralLatitude_56
             57 ->
               Just IntegralLatitude_57
             58 ->
               Just IntegralLatitude_58
             59 ->
               Just IntegralLatitude_59
             60 ->
               Just IntegralLatitude_60
             61 ->
               Just IntegralLatitude_61
             62 ->
               Just IntegralLatitude_62
             63 ->
               Just IntegralLatitude_63
             64 ->
               Just IntegralLatitude_64
             65 ->
               Just IntegralLatitude_65
             66 ->
               Just IntegralLatitude_66
             67 ->
               Just IntegralLatitude_67
             68 ->
               Just IntegralLatitude_68
             69 ->
               Just IntegralLatitude_69
             70 ->
               Just IntegralLatitude_70
             71 ->
               Just IntegralLatitude_71
             72 ->
               Just IntegralLatitude_72
             73 ->
               Just IntegralLatitude_73
             74 ->
               Just IntegralLatitude_74
             75 ->
               Just IntegralLatitude_75
             76 ->
               Just IntegralLatitude_76
             77 ->
               Just IntegralLatitude_77
             78 ->
               Just IntegralLatitude_78
             79 ->
               Just IntegralLatitude_79
             80 ->
               Just IntegralLatitude_80
             81 ->
               Just IntegralLatitude_81
             82 ->
               Just IntegralLatitude_82
             83 ->
               Just IntegralLatitude_83
             84 ->
               Just IntegralLatitude_84
             85 ->
               Just IntegralLatitude_85
             86 ->
               Just IntegralLatitude_86
             87 ->
               Just IntegralLatitude_87
             88 ->
               Just IntegralLatitude_88
             89 ->
               Just IntegralLatitude_89
             _ ->
               Nothing)

mod89IntegralLatitude01_89 ::
  Integral a =>
  a
  -> IntegralLatitude01_89
mod89IntegralLatitude01_89 n =
  let n' = n `mod` 89
  in fromMaybe (mod89IntegralLatitude01_89 n') (n' ^? integralLatitude01_89)

data IntegralLatitude =
  Equator
  | IntermediateLatitude
      Bool -- True is positive
      IntegralLatitude01_89
  deriving (Eq, Ord, Show)

integralLatitude ::
  Integral a =>
  Prism' a IntegralLatitude
integralLatitude =
  prism'
    (\l -> case l of
             Equator ->
               0
             IntermediateLatitude z t ->
               let r = integralLatitude01_89 # t
               in bool (-r) r z)
    (\l -> case l of
             0 ->
               Just Equator
             _ ->
               asum ((\(f, p) -> IntermediateLatitude p <$> (f l ^? integralLatitude01_89)) <$> [(id, True), (negate, False)]))

mod179IntegralLatitude ::
  Integral a =>
  a
  -> IntegralLatitude
mod179IntegralLatitude n =
  let n' = ((n + 89) `mod` 179) - 89
  in fromMaybe (mod179IntegralLatitude n') (n' ^? integralLatitude)

class Antipodal p f s where
  _Antipode ::
    Optic' p f s s

instance (Profunctor p, Functor f) => Antipodal p f IntegralLatitude where
  _Antipode =
    let neg Equator =
          Equator
        neg (IntermediateLatitude p z) =
          IntermediateLatitude (not p) z
    in  iso
          neg
          neg

class Equatorial p f s where
  _Equator ::
    Optic' p f s ()

instance (Choice p, Applicative f) => Equatorial p f IntegralLatitude where
  _Equator =
    prism'
      (\() -> Equator)
      (\l -> case l of
               Equator ->
                 Just ()
               IntermediateLatitude _ _ ->
                 Nothing)

data Latitude =
  Latitude IntegralLatitude Digits
  deriving (Eq, Ord, Show)

class AsLatitude p f s where
  _Latitude ::
    Optic' p f s Latitude

instance AsLatitude p f Latitude where
  _Latitude =
    id

instance (Profunctor p, Functor f) => AsLatitude p f (IntegralLatitude, Digits) where
  _Latitude =
    iso 
      (\(l, d) -> Latitude l d)
      (\(Latitude l d) -> (l, d))

instance (Profunctor p, Functor f) => AsLatitude p f (Digits, IntegralLatitude) where
  _Latitude =
    swapped . _Latitude

latitudeIntegral ::
  Lens'
    Latitude
    IntegralLatitude
latitudeIntegral =
  from (_Latitude :: Iso' (IntegralLatitude, Digits) Latitude) . _1

latitudeMantissa ::
  Lens'
    Latitude
    Digits
latitudeMantissa =
  from (_Latitude :: Iso' (IntegralLatitude, Digits) Latitude) . _2

instance (Profunctor p, Functor f) => Antipodal p f Latitude where
  _Antipode =
    let ap (Latitude l d) =
          Latitude (_Antipode # l) d
    in  iso
          ap
          ap

instance (Choice p, Applicative f) => Equatorial p f Latitude where
  _Equator =
    prism'
      (\() -> Latitude Equator undefined)
      (\(Latitude l d) -> if l == Equator && all (== x0) (digitsI # d) then Just () else Nothing)

data IntegralLongitude00x_17x =
  IntegralLongitude00x
  | IntegralLongitude01x
  | IntegralLongitude02x
  | IntegralLongitude03x
  | IntegralLongitude04x
  | IntegralLongitude05x
  | IntegralLongitude06x
  | IntegralLongitude07x
  | IntegralLongitude08x
  | IntegralLongitude09x
  | IntegralLongitude10x
  | IntegralLongitude11x
  | IntegralLongitude12x
  | IntegralLongitude13x
  | IntegralLongitude14x
  | IntegralLongitude15x
  | IntegralLongitude16x
  | IntegralLongitude17x
  deriving (Eq, Ord, Show) 

data IntegralLongitude000_179 =
  IntegralLongitude000_179
    IntegralLongitude00x_17x
    Digit
  deriving (Eq, Ord, Show) 

data IntegralLongitude =
  IntegralLongitude
    Bool
    IntegralLongitude000_179
  deriving (Eq, Ord, Show) 

data Longitude =
  Longitude
    IntegralLongitude
    Digits
  deriving (Eq, Ord, Show)

data FixedPoint =
  FixedPoint
    Latitude
    Longitude
  deriving (Eq, Ord, Show)

data Coordinate =
  Coordinate FixedPoint
  | NorthPole
  | SouthPole
  deriving (Eq, Ord, Show)

data Sixty0x_5x =
  Sixty0x_0x
  | Sixty0x_1x
  | Sixty0x_2x
  | Sixty0x_3x
  | Sixty0x_4x
  | Sixty0x_5x
  deriving (Eq, Ord, Show)

data Sixty =
  Sixty
    Sixty0x_5x
    Digit
  deriving (Eq, Ord, Show)
