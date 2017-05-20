module Main where

import Control.Coroutine as Co
import Control.Coroutine (runProcess, ($$))
import Control.Monad (class Monad)
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Show (class Show, show)
import Data.Time.Duration (Milliseconds(..))
import Effect.Mraa (Gpio, HWIO, Pin(..), dirOut, gpio, mraa, write)
import Math ((%))
import Prelude (Unit, bind, discard, pure, void, ($), (/))


main :: forall e. Eff (now :: NOW, hwio :: HWIO,  exception :: EXCEPTION, console :: CONSOLE | e) Unit
main = do
  let m = mraa
  ledPin <- gpio m $ Pin 44
  dirOut m ledPin
  void $ launchAff $ runProcess (timeStream $$ (sink ledPin))

showing :: forall a m. Show a => Monad m => Co.Transformer a String m Unit
showing = forever (Co.transform show)

sink :: forall eff. Gpio -> Co.Consumer Instant (Aff (hwio :: HWIO, console :: CONSOLE | eff)) Unit
sink led = forever do
  s <- Co.await
  lift (liftEff (onOff led s))
  pure Nothing

timeStream :: forall eff. Co.Producer Instant (Aff (now :: NOW | eff)) Unit
timeStream = do
  n <- lift (liftEff now)
  Co.emit n
  timeStream

onOff :: forall e. Gpio -> Instant -> Eff (hwio :: HWIO, console :: CONSOLE| e) Unit
onOff ledPin i = do
  let val = floor $ (fromMilli $ unInstant i)/1000.0 % 2.0
  logShow val
  write ledPin val
  where
  fromMilli (Milliseconds m) = m
