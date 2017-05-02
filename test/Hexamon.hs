module Main where

import Ivory.Tower.Config
import Ivory.Tower.Options
import Ivory.OS.FreeRTOS.Tower.STM32

import Hexamon.Platforms
import Hexamon.App (app)

main :: IO ()
main = compileTowerSTM32FreeRTOS testplatform_stm32 p $
    app (stm32config_clock . testplatform_stm32)
        testplatform_leds
        testplatform_dualhx711
        testplatform_can1
        testplatform_hexamon
  where
  p :: TOpts -> IO TestPlatform
  p topts = getConfig topts testPlatformParser
