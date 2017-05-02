{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Hexamon.App where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Peripheral.CAN.Filter

import Hexamon.Types

import Ivory.Serialize (packInto, serializeArtifacts, serializeModule)

import Hexamon.DualHX711
import Hexamon.Platforms
import Hexamon.Tests.LED

app :: (e -> ClockConfig) -> (e -> ColoredLEDs) -> (e -> DualHX711) -> (e -> TestCAN) -> (e -> HexamonConfig) -> Tower e ()
app tocc toleds todualhx711 totestcan tohexamon = do
  towerDepends serializeModule
  towerModule serializeModule

  mapM_ towerArtifact serializeArtifacts

  towerDepends hexamonTypes
  towerModule hexamonTypes

  can  <- fmap totestcan getEnv

  -- Serial ID extraction
  hexamonConfig <- fmap tohexamon getEnv
  let busunitSerial = hexamonConfigUID hexamonConfig

  -- leds <- fmap toleds    getEnv

  dualhx711Settings <- fmap todualhx711 getEnv
  (hx_chan0_out, hx_chan1_out) <- dualhx711Tower dualhx711Settings

  (res, req_mailbox0, req_mailbox1, _) <- canTower tocc (testCAN can) 125000 (testCANRX can) (testCANTX can)

  monitor "simplecontroller" $ do
    handler systemInit "init" $ do
      callback $ const $ do
        let emptyID = CANFilterID32 (fromRep 0) (fromRep 0) False False
        canFilterInit (testCANFilters can)
                      [CANFilterBank CANFIFO0 CANFilterMask $ CANFilter32 emptyID emptyID]
                      []
        -- ledSetup $ redLED leds
        -- ledOn    $ redLED leds

    tx0_pending <- state "tx0_pending"
    tx1_pending <- state "tx1_pending"
    last_sent  <- state "last_sent"

    -- CAN Mailbox 0
    handler hx_chan0_out "hx_chan0_out" $ do
      abort_emitter <- emitter (abortableAbort    req_mailbox0) 1
      req_emitter   <- emitter (abortableTransmit req_mailbox0) 1
      callback $ \sensorSample -> do
        let msgid = extendedCANID (fromRep busunitSerial) (boolToBit false)
        r <- local $ istruct
                [ can_message_id  .= ival msgid
                , can_message_len .= ival 6
                ]
        sensorIndex <- local $ ival (0 :: Uint8)
        packInto (r~>can_message_buf) 0 (constRef sensorIndex)
        packInto (r~>can_message_buf) 2 (sensorSample~>sensor_value)
        refCopy last_sent r

        was_pending <- deref tx0_pending
        ifte_ was_pending (emitV abort_emitter true) $ do
          emit req_emitter $ constRef last_sent
          store tx0_pending true

    handler (abortableComplete req_mailbox0) "tx0_complete" $ do
      req_emitter <- emitter (abortableTransmit req_mailbox0) 1
      callbackV $ \ ok -> do
        ifte_ ok (store tx0_pending false) $ do
          emit req_emitter $ constRef last_sent
          store tx0_pending true

    -- CAN Mailbox 1
    handler hx_chan1_out "hx_chan1_out" $ do
      abort_emitter <- emitter (abortableAbort    req_mailbox1) 1
      req_emitter   <- emitter (abortableTransmit req_mailbox1) 1
      callback $ \sensorSample -> do
        let msgid = extendedCANID (fromRep busunitSerial) (boolToBit false)
        r <- local $ istruct
                [ can_message_id  .= ival msgid
                , can_message_len .= ival 6
                ]
        sensorIndex <- local $ ival (1 :: Uint8)
        packInto (r~>can_message_buf) 0 (constRef sensorIndex)
        packInto (r~>can_message_buf) 2 (sensorSample~>sensor_value)
        refCopy last_sent r

        was_pending <- deref tx1_pending
        ifte_ was_pending (emitV abort_emitter true) $ do
          emit req_emitter $ constRef last_sent
          store tx1_pending true

    handler (abortableComplete req_mailbox1) "tx1_complete" $ do
      req_emitter <- emitter (abortableTransmit req_mailbox1) 1
      callbackV $ \ ok -> do
        ifte_ ok (store tx1_pending false) $ do
          emit req_emitter $ constRef last_sent
          store tx1_pending true

    -- Not enough SRAM for LEDs and CAN processing on STM32F042 :(

    -- received <- stateInit "can_received_count" (ival (0 :: Uint32))
    -- handler res "result" $ do
    --   callback $ const $ do
    --     count <- deref received
    --     store received (count + 1)
    --     ifte_ (count .& 1 ==? 0)
    --       (ledOff $ redLED leds)
    --       (ledOn  $ redLED leds)
