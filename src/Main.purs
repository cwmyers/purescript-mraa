module Main where


import Control.Coroutine as Co
import Control.Coroutine (runProcess, ($$))
import Control.Monad (class Monad)
import Control.Monad.Aff (Aff, delay, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Show (class Show, show)
import Effect.Mraa (Gpio, HWIO, Pin(..), dirOut, gpio, mraa, write)
import Math ((%))
import Prelude (Unit, bind, discard, pure, void, ($), (+))


main :: forall e. Eff (hwio :: HWIO,  exception :: EXCEPTION, console :: CONSOLE | e) Unit
main = do
  let m = mraa
  ledPin <- gpio m $ Pin 44
  dirOut m ledPin
  void $ launchAff $ runProcess (nats $$ (sink ledPin))

showing :: forall a m. Show a => Monad m => Co.Transformer a String m Unit
showing = forever (Co.transform show)

sink :: forall eff. Gpio -> Co.Consumer Int (Aff (hwio :: HWIO, console :: CONSOLE | eff)) Unit
sink led = forever do
  s <- Co.await
  lift (liftEff (onOff led s))
  pure Nothing

nats :: forall eff. Co.Producer Int (Aff eff) Unit
nats = go 0
  where
  go i = do
    Co.emit i
    lift (delay (wrap 1000.0)) -- 10ms delay
    go (i + 1)

-- timeStream :: forall eff. Co.Producer Instant (Eff _) Instant
-- timeStream = producer $ now `mapFlipped` Right

--rebind :: forall e a. Instant -> Run ( base :: FProxy (Eff( now :: NOW| a))| e) Unit
-- rebind n = liftBase now

-- forevs :: forall a e. Run ( base :: FProxy (Eff( now :: NOW| a))| e) Instant
-- forevs = forever $ liftBase ( now )

-- showN n = liftBase $ log "hi"

-- xxx :: forall a. Run (  a )


onOff :: forall e. Gpio -> Int -> Eff (hwio :: HWIO, console :: CONSOLE| e) Unit
onOff ledPin i = do
  let val = floor $ (toNumber i) % (toNumber 2)
  logShow val
  write ledPin val
