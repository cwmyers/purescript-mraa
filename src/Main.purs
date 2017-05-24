module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW, now)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Int (floor, toNumber)
import Data.Time.Duration (Milliseconds(..))
import Effect.FakeMraa (Gpio, HWIO, Pin(..), dirOut, gpio, mraa, write)
import Math ((%))
import Prelude (Unit, bind, discard, pure, void, (#), ($), (+), (<<<), (>=))
import Run (Run, liftBase, runBase)
import Run.Streaming.Prelude (foldM, produce)

type PinState = { pin :: Gpio, lastUpdate :: Number, lastValue :: Int}
type World = { pinState :: PinState }

main :: forall e. Eff (now :: NOW, hwio :: HWIO, exception :: EXCEPTION, console :: CONSOLE | e) Unit
main = do
  let m = mraa
  ledPin <- gpio m $ Pin 44
  dirOut m ledPin
  let timeStream = produce $ liftBase now
  let world = { pinState : {pin : ledPin, lastUpdate : 0.0 , lastValue : 0}}
  timeStream # (runIt $ world) # runBase # void

runIt :: World -> Run (_) Unit -> Run (_) World
runIt world = foldM (\w i -> liftBase $ process w i) (pure world) pure

process :: forall eff.
  World ->
  Instant ->
  Eff ( console :: CONSOLE, hwio :: HWIO | eff )  World
process world instant = if i >= (world.pinState.lastUpdate + 1000.0)
  then do
    logShow i
    let newValue = floor $ toNumber (world.pinState.lastValue + 1) % 2.0
    write world.pinState.pin newValue
    pure ({pinState: { pin: world.pinState.pin, lastUpdate: i, lastValue: newValue}})
  else
    pure world
  where
    i = instantToNumber instant

instantToNumber :: Instant -> Number
instantToNumber = fromMilli <<< unInstant

fromMilli :: Milliseconds -> Number
fromMilli (Milliseconds m) = m
