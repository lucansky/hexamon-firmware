
module Main where

import Ivory.Tower.Config
import Ivory.Tower.Options
import Ivory.OS.FreeRTOS.Tower.STM32

import Hello.Tests.Platforms
import Hello.Tests.Blink (app)

main :: IO ()
main = compileTowerSTM32FreeRTOS testplatform_stm32 p $
  app testplatform_leds
  where
  p :: TOpts -> IO TestPlatform
  p topts = getConfig topts testPlatformParser

