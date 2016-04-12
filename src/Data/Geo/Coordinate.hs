module Data.Geo.Coordinate ({- module C, -} IntegralLatitude01_89(..), IntegralLatitude(..), IntegralLongitude00x_17x(..), IntegralLongitude000_179(..), IntegralLongitude(..)) where

import Prelude
import Data.Digit

{-
import Data.Geo.Coordinate.Coordinate as C
import Data.Geo.Coordinate.DegreesLatitude as C
import Data.Geo.Coordinate.DegreesLongitude as C
import Data.Geo.Coordinate.Latitude as C
import Data.Geo.Coordinate.Longitude as C
import Data.Geo.Coordinate.Minutes as C
import Data.Geo.Coordinate.Seconds as C
-}

{-
modIntegralLatitude01_89 :: Integral a => a -> IntegralLatitude01_89
integralLatitude01_89 :: Integral a => Prism a IntegralLatitude01_89
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

{-
modIntegralLatitude :: Integral a => a -> IntegralLatitude
integralLatitude :: Integral a => Prism a IntegralLatitude
antipodeIntegralLatitude :: Iso IntegralLatitude IntegralLatitude
notequator :: Prism IntegralLatitude (Bool, IntegralLatitude01_89)
equator :: Prism IntegralLatitude ()
-}
data IntegralLatitude =
  Equator
  | IntermediateLatitude Bool IntegralLatitude01_89
  deriving (Eq, Ord, Show)

{-
antipodeLatitude :: Iso Latitude Latitude
integralLatitudeL :: Lens Latitude IntegralLatitude
mantissLatitude :: Lens Latitude Digits
-}
data Latitude =
  Latitude IntegralLatitude Digits
  deriving (Eq, Ord, Show)  

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
