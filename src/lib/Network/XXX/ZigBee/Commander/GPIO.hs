{-

This file is part of the zigbee-commander package. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/zbc/LICENSE. No part of
the zigbee-commander package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

module Network.XXX.ZigBee.Commander.GPIO
  ( GPIO (..),
    DigitalState (..),
    PinState (..),
    PinID,
  )
where

data DigitalState = DigitalLow | DigitalHigh deriving (Show, Eq, Enum)

type PinID = Int

data GPIO = GPIO
  { gpioName :: PinID,
    gpioState :: PinState
  }

data PinState
  = Disabled
  | DigitalInput
  | DigitalOutput
  | AnalogInput
  | SpecialPurpose
