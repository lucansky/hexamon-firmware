{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Hexamon.HX711 where

import Ivory.Language
import Ivory.HW
import Ivory.Tower

-- import Ivory.BSP.STM32F042.RCC

-- import Ivory.BSP.STM32.Interrupt
-- import Ivory.BSP.STM32.ClockConfig
-- import Ivory.BSP.STM32.Driver.UART
-- import Ivory.BSP.STM32.Peripheral.ADC
-- import Ivory.BSP.STM32.Peripheral.ATIM18
import Ivory.BSP.STM32.Peripheral.GPIOF4
-- import Ivory.BSP.STM32.Peripheral.SYSCFG
-- import Ivory.BSP.STM32.Peripheral.EXTI
-- import Ivory.BSP.STM32.Peripheral.RCC

import Hexamon.Platforms
import Hexamon.Types

hx711Tower :: HX711 -> Tower e (ChanOutput ('Struct "sensor_sample"))
hx711Tower HX711{clockPin=clk, dataPin=dat} = do
  -- periodic <- period (Milliseconds 500)

  --let SYSCFG{..} = syscfg
  --let EXTI{..} = exti

  periodic <- period (Milliseconds 500)

  chan <- channel

  monitor "hx711" $ do
    monitorModuleDef $ hw_moduledef

    current_weight <- stateInit "current_weight" (ival (0 :: Uint32))

    -- weight_scaled <- stateInit "weight_scaled" (ival (0.0 :: IFloat))
  --
    handler periodic "periodic_hx711" $ do
      e <- emitter (fst chan) 1
      callback $ const $ do
        --emitV e true
        store current_weight 0

        arrayMap $ \(i :: Ix 24) -> do
          arrayMap $ \(_ :: Ix 10) -> pinSet clk

          val <- (pinRead dat)
          currenter_weight <- deref current_weight
          store current_weight $ currenter_weight  .| ((safeCast val) `iShiftL` (24 - signCast (fromIx i)))

          --store current_weight $ currenter_weight  + (safeCast val)

          arrayMap $ \(_ :: Ix 10) -> pinClear clk

        arrayMap $ \(_ :: Ix 1) -> do
          arrayMap $ \(_ :: Ix 10) -> pinSet clk
          arrayMap $ \(_ :: Ix 10) -> pinClear clk

        measured_weight <- deref current_weight
        -- store weight_scaled $ ((safeCast :: Uint32 -> IFloat) measured_weight - 140772.0 ) / 85.3

        x <- local $ istruct
                [ sensor_type  .= ival (0 :: Uint8)
                , sensor_index .= ival (1 :: Uint8)
                , sensor_value .= ival measured_weight ]
        emit e (constRef x)

    handler systemInit "init" $ do
      callback $ const $ do
        set_output_pin clk
        set_input_pin dat

  return (snd chan)

set_output_pin :: GPIOPin -> Ivory eff ()
set_output_pin p = do
  pinEnable   p
  pinSetMode  p gpio_mode_output
  pinSetPUPD  p gpio_pupd_pullup
  pinSetSpeed p gpio_speed_2mhz

set_input_pin :: GPIOPin -> Ivory eff ()
set_input_pin p = do
  pinEnable   p
  pinSetMode  p gpio_mode_input
  pinSetPUPD  p gpio_pupd_none
  pinSetSpeed p gpio_speed_2mhz

app :: (e -> ColoredLEDs) -> (e -> HX711) -> Tower e ()
app toleds tohx711 = do
  -- leds <- fmap toleds getEnv
  hx711Settings <- fmap tohx711 getEnv

  _ <- hx711Tower hx711Settings

  return ()
  -- blink (Milliseconds 1000) [redLED leds]
  -- blink (Milliseconds 666) [blueLED leds]
