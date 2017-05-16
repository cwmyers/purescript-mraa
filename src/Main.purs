module Main where

import Prelude
import Control.Monad.Eff (Eff)
-- import Control.Monad.Aff (delay)

import Effect.Mraa (HWIO, Pin(..), dirOut, gpio, mraa, write)

main :: forall e. Eff (hwio :: HWIO | e) Unit
main = do
  let m = mraa
  ledPin <- gpio m $ Pin 44
  dirOut m ledPin
  write ledPin 1
