module Effect.Mraa where

import Control.Monad.Eff (kind Effect, Eff)
import Data.Unit (Unit)

newtype Pin = Pin Int

foreign import data Mraa :: Type
foreign import data Gpio :: Type
foreign import data HWIO :: Effect

foreign import mraa :: Mraa

foreign import gpio :: forall eff.
                       Mraa -> Pin -> Eff (hwio :: HWIO | eff) Gpio

foreign import write :: forall eff. Gpio -> Int -> Eff (hwio :: HWIO | eff) Unit

foreign import dirOut :: forall eff. Mraa -> Gpio -> Eff (hwio :: HWIO | eff) Unit
