module Process where

import Prelude
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW, now)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Time.Duration (Milliseconds(..))
import Effect.Mraa (HWIO)
import Run (FProxy, Run, liftBase)
import Run.Streaming (Producer)
import Run.Streaming.Prelude (foldM)

type MyBase a r = ( base :: FProxy a | r )
type MyAff r = Aff( console :: CONSOLE, hwio :: HWIO | r)


sleep :: forall a b.
  Number -> Run ( MyBase (Aff ( now :: NOW  | b )) a ) Instant
sleep sleepTime = do
  liftBase $ delay $ Milliseconds sleepTime
  liftBase $ liftEff now


runIt :: forall world a r. world ->
  (world -> Instant -> Eff ( console :: CONSOLE, hwio :: HWIO | a )  world)
  -> Run ( Producer Instant ( MyBase (MyAff a) r)) Unit
     -> Run ( MyBase (MyAff a) r ) world
runIt world f = foldM (\w i -> liftBase $ liftEff $ f w i) (pure world) pure

runItAff :: forall world a r. world ->
  (world -> Instant -> Aff ( console :: CONSOLE, hwio :: HWIO | a )  world)
  -> Run ( Producer Instant ( MyBase (MyAff a) r)) Unit
     -> Run ( MyBase (MyAff a) r ) world
runItAff world f = foldM (\w i -> liftBase $ f w i) (pure world) pure


instantToNumber :: Instant -> Number
instantToNumber = fromMilli <<< unInstant

fromMilli :: Milliseconds -> Number
fromMilli (Milliseconds m) = m
