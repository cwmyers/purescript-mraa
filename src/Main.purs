module Main where

import Control.Monad.Aff (Aff, delay, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Data.DateTime.Instant (Instant)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Mraa (BinValue(Low, High), Direction(..), Gpio, HWIO, Pin(Pin), dir, gpio, read, write)
import Prelude (Unit, bind, discard, pure, void, (#), ($), (+), (>=))
import Process (instantToNumber, runItAff, sleep)
import Run (runBase)
import Run.Streaming.Prelude (produce)

type PinState = { pin :: Gpio, lastUpdate :: Number, state :: Int, nextUpdate :: Number}
type World = { pinState :: PinState }

type TransitionState = { nextState :: Int, pinState :: BinValue, delay :: Number }

defaultState :: TransitionState
defaultState = { nextState : 0, pinState : Low, delay : 1000.0}

states :: Map Int TransitionState
states = fromFoldable [
  Tuple 0 { nextState : 1, pinState : High, delay : 10.0},
  Tuple 1 { nextState : 2, pinState : Low, delay : 10.0},
  Tuple 2 { nextState : 3, pinState : High, delay : 10.0},
  Tuple 3 { nextState : 0, pinState : Low, delay : 10.0}
  ]

main :: forall e. Eff (now :: NOW, hwio :: HWIO, exception :: EXCEPTION, console :: CONSOLE | e) Unit
main = do
  ledPin <- gpio $ Pin 44
  ledPin2 <- gpio $ Pin 45
  dir ledPin Out
  dir ledPin2 In
  v <- read ledPin2
  logShow v
  let timeStream = produce $ sleep 0.0
  let world = { pinState : {pin : ledPin, lastUpdate : 0.0 , state : 0, nextUpdate: 0.0}}
  timeStream # runItAff world myProj # runBase # launchAff # void


myProj :: forall eff.
  World
  -> Instant
     -> Aff ( console :: CONSOLE, hwio :: HWIO | eff) World
myProj w i = do
  world <- onOff w i
  liftEff $ process world i

onOff :: forall eff.
  World -> Instant -> Aff (console:: CONSOLE, hwio :: HWIO | eff) World
onOff world instant = do
  liftEff $ write world.pinState.pin High
  delay $ Milliseconds 1000.0
  liftEff $ write world.pinState.pin Low
  delay $ Milliseconds 1000.0
  pure world


process :: forall eff.
  World ->
  Instant ->
  Eff ( console :: CONSOLE, hwio :: HWIO | eff )  World
process world instant = if currentTime >= world.pinState.nextUpdate
  then do
    let state = fromMaybe defaultState $ lookup world.pinState.state states
    write world.pinState.pin state.pinState
    let nextUpdate = currentTime + state.delay
    pure ({pinState: { pin: world.pinState.pin, lastUpdate: currentTime,
      state: state.nextState, nextUpdate: nextUpdate}})
  else
    pure world
  where
    currentTime = instantToNumber instant
