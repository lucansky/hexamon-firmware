{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Hello.Tests.Platforms
  ( testPlatformParser
  , ColoredLEDs(..)
  , TestUART(..)
  , TestSPI(..)
  , TestI2C(..)
  , TestCAN(..)
  , TestDMA(..)
  , TestPlatform(..)
  , testplatform_clockconfig
  , hello4disco
  ) where

import Ivory.Tower.Config
import Data.Char (toUpper)

import qualified Ivory.BSP.STM32F405.CAN         as F405
import qualified Ivory.BSP.STM32F405.UART        as F405
import qualified Ivory.BSP.STM32F405.GPIO        as F405
import qualified Ivory.BSP.STM32F405.GPIO.AF     as F405
import qualified Ivory.BSP.STM32F405.SPI         as F405
import qualified Ivory.BSP.STM32F405.I2C         as F405
import qualified Ivory.BSP.STM32F405.RNG         as F405

import Ivory.BSP.STM32.Peripheral.CAN
import Ivory.BSP.STM32.Peripheral.GPIOF4
import Ivory.BSP.STM32.Peripheral.UART
import Ivory.BSP.STM32.Peripheral.SPI hiding (ActiveHigh, ActiveLow)
import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32.Peripheral.RNG
import Ivory.BSP.STM32.Peripheral.UART.DMA
import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Config

import Hello.Tests.LED

testPlatformParser :: ConfigParser TestPlatform
testPlatformParser = do
  p <- subsection "args" $ subsection "platform" string
  case map toUpper p of
    "HELLO4DISCO"       -> result hello4disco
--    "HELLOF0"           -> result hellof0
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

data TestPlatform =
  TestPlatform
    { testplatform_leds  :: ColoredLEDs
    , testplatform_uart  :: TestUART
    , testplatform_i2c   :: TestI2C
    , testplatform_can1  :: TestCAN
    , testplatform_rng   :: RNG
    , testplatform_stm32 :: STM32Config
    }

testplatform_clockconfig :: TestPlatform -> ClockConfig
testplatform_clockconfig = stm32config_clock . testplatform_stm32

-- HELLO4DISCO

hello4disco :: TestPlatform
hello4disco = TestPlatform
  { testplatform_leds = ColoredLEDs
      { redLED  = LED F405.pinD14 ActiveHigh
      , blueLED = LED F405.pinD15 ActiveHigh
      }
  , testplatform_uart = TestUART
    { testUARTPeriph = F405.uart2
    , testUARTPins = UARTPins
        { uartPinTx = F405.pinA2
        , uartPinRx = F405.pinA3
        , uartPinAF = F405.gpio_af_uart2
        }
    }
  , testplatform_i2c = TestI2C
      { testI2C = F405.i2c2
      , testI2CPins = I2CPins
        { i2cpins_sda = F405.pinB11
        , i2cpins_scl = F405.pinB10
        }
      }
  , testplatform_can1 = TestCAN
      { testCAN = F405.can1
      , testCANRX = F405.pinB8
      , testCANTX = F405.pinB9
      , testCANFilters = F405.canFilters
      }
  , testplatform_rng = F405.rng
  , testplatform_stm32 = stm32f405Defaults 8
  }
