module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW, now)
import Run (Run, liftBase, runBase)
import Run.Streaming.Prelude (forever)


main :: forall e. Eff ( now :: NOW, console :: CONSOLE | e) Unit
main = do
  -- let m = mraa
  -- ledPin <- gpio m $ Pin 44
  -- dirOut m ledPin
  -- runBase $ forever $ liftBase (onOff ledPin)
  runBase $ forever $ liftBase ( now )

-- xxx :: forall a. Run (  a )


-- onOff :: forall e. Gpio -> Eff (hwio :: HWIO, console :: CONSOLE| e) Unit
-- onOff ledPin = do
--   log "Turning on led"
--   write ledPin 1
--   log "Turning off led"
--   write ledPin 0
