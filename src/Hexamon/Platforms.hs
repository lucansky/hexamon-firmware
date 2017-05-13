{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Hexamon.Platforms
  ( testPlatformParser
  , ColoredLEDs(..)
  , TestUART(..)
  , TestSPI(..)
  , TestI2C(..)
  , TestCAN(..)
  , TestDMA(..)
  , HX711(..)
  , DualHX711(..)
  , HexamonConfig(..)
  , TestPlatform(..)
  , testplatform_clockconfig
  , hellof0
  ) where

import Ivory.Tower.Config
import Data.Char (toUpper)
import Ivory.Language

import qualified Ivory.BSP.STM32F042.GPIO        as F042
import qualified Ivory.BSP.STM32F042.CAN         as F042

import Ivory.BSP.STM32.Peripheral.CAN
import Ivory.BSP.STM32.Peripheral.GPIOF4
import Ivory.BSP.STM32.Peripheral.UART
import Ivory.BSP.STM32.Peripheral.SPI hiding (ActiveHigh, ActiveLow)
import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32.Peripheral.UART.DMA
import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Config

import Hexamon.Tests.LED

testPlatformParser :: ConfigParser TestPlatform
testPlatformParser = do
  p <- subsection "args" $ subsection "platform" string
  case map toUpper p of
    "HELLOF0"           -> result hellof0
    _ -> fail ("no such platform " ++ p)

  where
  result platform = do
    conf <- stm32ConfigParser (testplatform_stm32 platform)
    return platform { testplatform_stm32 = conf }

data ColoredLEDs =
  ColoredLEDs
    { redLED  :: LED
    , blueLED :: LED
    }

data HX711 =
  HX711
    { clockPin :: GPIOPin
    , dataPin  :: GPIOPin
    }

data DualHX711 =
  DualHX711
    { sharedClockPin :: GPIOPin
    , dataPin0 :: GPIOPin
    , dataPin1 :: GPIOPin
    }

-- data MultiHX711 =
--   MultiHX711
--     { sharedClockPin :: GPIOPin
--     , dataPins  :: [GPIOPin]
--     }

data TestUART =
  TestUART
    { testUARTPeriph :: UART
    , testUARTPins   :: UARTPins
    }

data TestSPI =
  TestSPI
    { testSPIPeriph :: SPIPeriph
    , testSPIPins   :: SPIPins
    -- TODO FIXME: move CS pins for test devices into TestSPI
    }

data TestI2C =
  TestI2C
    { testI2C     :: I2CPeriph
    , testI2CPins :: I2CPins
    }

data TestCAN =
  TestCAN
    { testCAN        :: CANPeriph
    , testCANRX      :: GPIOPin
    , testCANTX      :: GPIOPin
    , testCANFilters :: CANPeriphFilters
    }

data TestDMA =
  TestDMA
    { testDMAUARTPeriph :: DMAUART
    , testDMAUARTPins   :: UARTPins
    }

data HexamonConfig =
  HexamonConfig
    { hexamonConfigUID  :: Uint32
    }

data TestPlatform =
  TestPlatform
    { testplatform_leds       :: ColoredLEDs
    , testplatform_dualhx711  :: DualHX711
    , testplatform_can1       :: TestCAN
    , testplatform_hexamon    :: HexamonConfig
    , testplatform_stm32      :: STM32Config
    }

testplatform_clockconfig :: TestPlatform -> ClockConfig
testplatform_clockconfig = stm32config_clock . testplatform_stm32

-- HELLO0DISCO

hellof0 :: TestPlatform
hellof0 = TestPlatform
  { testplatform_leds = ColoredLEDs
      { redLED  = LED F042.pinA0 ActiveHigh
      , blueLED = LED F042.pinA3 ActiveHigh
      }
  , testplatform_dualhx711 = DualHX711
      { sharedClockPin = F042.pinA1
      , dataPin0       = F042.pinA5
      , dataPin1       = F042.pinA4
      }
  , testplatform_can1 = TestCAN
      { testCAN = F042.can1
      , testCANRX = F042.pinA11
      , testCANTX = F042.pinA12
      , testCANFilters = F042.canFilters
      }
  -- Here edit UID of the bus-unit
  , testplatform_hexamon = HexamonConfig { hexamonConfigUID = 1450055 }
  , testplatform_stm32 = stm32f042Defaults 8
  }
