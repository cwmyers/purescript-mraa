module Main where

import Control.Monad.Aff (Aff, delay, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW, now)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.FakeMraa (Gpio, HWIO, Pin(..), dirOut, gpio, mraa, write)
import Prelude (Unit, bind, discard, pure, void, (#), ($), (+), (<<<), (>=))
import Run (FProxy, Run, liftBase, runBase)
import Run.Streaming (Producer)
import Run.Streaming.Prelude (foldM, produce)

type PinState = { pin :: Gpio, lastUpdate :: Number, state :: Int, nextUpdate :: Number}
type World = { pinState :: PinState }

type TransitionState = { nextState :: Int, pinState :: Int, delay :: Number }

defaultState :: TransitionState
defaultState = { nextState : 0, pinState : 0, delay : 1000.0}

states :: Map Int TransitionState
states = fromFoldable [
  Tuple 0 { nextState : 1, pinState : 1, delay : 10.0},
  Tuple 1 { nextState : 2, pinState : 0, delay : 10.0},
  Tuple 2 { nextState : 3, pinState : 1, delay : 10.0},
  Tuple 3 { nextState : 0, pinState : 0, delay : 10.0}
  ]

main :: forall e. Eff (now :: NOW, hwio :: HWIO, exception :: EXCEPTION, console :: CONSOLE | e) Unit
main = do
  let m = mraa
  ledPin <- gpio m $ Pin 44
  dirOut m ledPin
  let timeStream = produce $ sleep
  let world = { pinState : {pin : ledPin, lastUpdate : 0.0 , state : 0, nextUpdate: 0.0}}
  timeStream # (runIt $ world) # runBase # launchAff # void

type MyAff r = Aff( console :: CONSOLE, hwio :: HWIO | r)

type MyBase a r = ( base :: FProxy a | r )

runIt :: forall a r. World -> Run ( Producer Instant ( MyBase (MyAff a) r)) Unit
     -> Run ( MyBase (MyAff a) r ) World
runIt world = foldM (\w i -> liftBase $ liftEff $ process w i) (pure world) pure

sleep :: forall a b.
  Run ( MyBase (Aff ( now :: NOW  | b )) a ) Instant
sleep = do
  time <- liftBase $ liftEff now
  liftBase $ delay $ Milliseconds 10.0
  pure time

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

instantToNumber :: Instant -> Number
instantToNumber = fromMilli <<< unInstant

fromMilli :: Milliseconds -> Number
fromMilli (Milliseconds m) = m
