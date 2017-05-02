{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Hexamon.DualHX711 where

import Ivory.Language
-- import Ivory.Stdlib
import Ivory.HW
import Ivory.Tower

import Ivory.BSP.STM32.Peripheral.GPIOF4

import Hexamon.Platforms
import Hexamon.Types

dualhx711Tower :: DualHX711
                        -> Tower e
                             (ChanOutput ('Struct "sensor_sample"), ChanOutput ('Struct "sensor_sample"))
dualhx711Tower DualHX711{sharedClockPin=clk, dataPin0=dat0, dataPin1=dat1} = do
  -- periodic <- period (Milliseconds 500)

  --let SYSCFG{..} = syscfg
  --let EXTI{..} = exti

  periodic <- period (Milliseconds 500)

  chan0 <- channel
  chan1 <- channel

  monitor "hx711" $ do
    monitorModuleDef $ hw_moduledef

    current_weight0 <- stateInit "current_weight0" (ival (0 :: Uint32))
    current_weight1 <- stateInit "current_weight1" (ival (0 :: Uint32))


    handler periodic "periodic_hx711" $ do
      e0 <- emitter (fst chan0) 1
      e1 <- emitter (fst chan1) 1
      callback $ const $ do
        --emitV e true
        store current_weight0 0
        store current_weight1 0

        arrayMap $ \(i :: Ix 24) -> do
          arrayMap $ \(_ :: Ix 20) -> pinSet clk

          val0 <- (pinRead dat0)
          val1 <- (pinRead dat1)
          aux_weight0 <- deref current_weight0
          aux_weight1 <- deref current_weight1
          store current_weight0 $ aux_weight0  .| ((safeCast val0) `iShiftL` (24 - signCast (fromIx i)))
          store current_weight1 $ aux_weight1  .| ((safeCast val1) `iShiftL` (24 - signCast (fromIx i)))

          arrayMap $ \(_ :: Ix 20) -> pinClear clk

        -- One CLK pulse to set 128x Channel A gain
        arrayMap $ \(_ :: Ix 1) -> do
          arrayMap $ \(_ :: Ix 20) -> pinSet clk
          arrayMap $ \(_ :: Ix 20) -> pinClear clk

        measured_weight0 <- deref current_weight0
        measured_weight1 <- deref current_weight1
        -- store weight_scaled $ ((safeCast :: Uint32 -> IFloat) measured_weight - 140772.0 ) / 85.3

        x0 <- local $ istruct
                [ sensor_type  .= ival (0 :: Uint8)
                , sensor_index .= ival (1 :: Uint8)
                , sensor_value .= ival measured_weight0 ]
        emit e0 (constRef x0)


        x1 <- local $ istruct
                [ sensor_type  .= ival (0 :: Uint8)
                , sensor_index .= ival (1 :: Uint8)
                , sensor_value .= ival measured_weight1 ]
        emit e1 (constRef x1)

    handler systemInit "init" $ do
      callback $ const $ do
        set_output_pin clk
        set_input_pin dat0
        set_input_pin dat1

  return (snd chan0, snd chan1)

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
  pinSetPUPD  p gpio_pupd_pulldown
  pinSetSpeed p gpio_speed_2mhz
