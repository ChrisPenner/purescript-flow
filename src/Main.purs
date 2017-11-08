module Main where

import Prelude

import Control.Monad.Aff (Aff, delay, runAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Rec.Class (forever)
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (Variant, default, inj, on)
import Flow (flow)
import Pipes.Aff as P
import Type.Prelude (SProxy(..))

type AppEvents = Variant (tick :: Tick)

instance showAppState :: Show AppState where
  show (AppState n) = "AppState " <> (show n)

data AppState = AppState Int

reducer :: forall evts
  . AppState
  -> Variant (tick :: Tick | evts)
  -> AppState
reducer s@(AppState n) =
  default s
  # on _tick (\Tick -> AppState (n + 1))

data Tick = Tick

_tick :: SProxy "tick"
_tick = SProxy

tickEvent :: forall v. Variant (tick :: Tick | v)
tickEvent = inj _tick Tick

ticker :: forall evts eff
  . P.Output (Variant (tick :: Tick | evts))
  -> Aff (avar :: AVAR | eff) Unit
ticker out = forever do
  _ <- P.send' tickEvent out
  delay (Milliseconds 1000.0)

ignoreErrors :: forall a f. Applicative f => a -> f Unit
ignoreErrors = const $ pure unit

main :: forall eff. Eff (avar :: AVAR, console :: CONSOLE | eff) Unit
main = pure unit <* (runAff ignoreErrors
       (flow reducer [ticker] render startState))
  where
    render = log <<< show
    startState = AppState 0

