module Main where

import Prelude
import Effect.Mraa
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let m = mraa
  ledPin <- gpio m 44
  dirOut m ledPin
