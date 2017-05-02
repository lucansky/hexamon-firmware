{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}


module Hexamon.Types where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

[ivory|
struct sensor_sample
  { sensor_type   :: Stored Uint8
  ; sensor_index  :: Stored Uint8
  ; sensor_value  :: Stored Uint32
  }
|]

hexamonTypes :: Module
hexamonTypes = package "hexamon_types" $ do
  defStruct (Proxy :: Proxy "sensor_sample")
