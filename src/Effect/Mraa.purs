module Effect.Mraa(
  Pin(..),
  BinValue(..),
  Direction(..),
  Gpio,
  HWIO,
  gpio,
  read,
  write,
  dir
  ) where

import Prelude
import Control.Monad.Eff (kind Effect, Eff)

newtype Pin = Pin Int

data BinValue = High | Low

binValueToInt :: BinValue -> Int
binValueToInt High = 1
binValueToInt Low  = 0

intToBinValue :: Int -> BinValue
intToBinValue 1 = High
intToBinValue _ = Low

data Direction = In | Out

dirToInt :: Direction -> Int
dirToInt Out = dirOut
dirToInt In = dirIn

foreign import data Gpio :: Type
foreign import data HWIO :: Effect

foreign import dirOut :: Int
foreign import dirIn :: Int

foreign import gpio :: forall eff. Pin -> Eff (hwio :: HWIO | eff) Gpio

read :: forall eff. Gpio -> Eff (hwio :: HWIO | eff) BinValue
read pin = intToBinValue <$> readPriv pin

foreign import readPriv :: forall eff. Gpio -> Eff (hwio :: HWIO | eff) Int

write :: forall eff. Gpio -> BinValue -> Eff (hwio :: HWIO | eff) Unit
write pin value = writePriv pin $ binValueToInt value

foreign import writePriv :: forall eff. Gpio -> Int -> Eff (hwio :: HWIO | eff) Unit

dir :: forall eff. Gpio -> Direction -> Eff (hwio :: HWIO | eff) Unit
dir pin direction = dirPriv pin $ dirToInt direction

foreign import dirPriv :: forall eff. Gpio -> Int -> Eff (hwio :: HWIO | eff) Unit

instance showBinValue :: Show BinValue where
  show High = "High"
  show Low  = "Low"
