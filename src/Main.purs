module Main where

import Prelude

import Control.Monad.Aff (Aff, delay, runAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Rec.Class (forever)
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (Variant, inj)
import Flow (flow)
import Flow.Events (class EventHandler)
import Pipes.Aff as P
import Type.Prelude (SProxy(..))

data AppState = AppState Int

instance showAppState :: Show AppState where
  show (AppState n) = "AppState " <> (show n)

instance eventHandlerAppState :: EventHandler AppState where
  handle (AppState n) _ = AppState (n + 1)


data Tick = Tick

tickEvent :: forall v. Variant (tick :: Tick | v)
tickEvent = inj (SProxy :: SProxy "tick") Tick

ticker :: forall evts eff
  . P.Output (Variant (tick :: Tick | evts))
  -> Aff (avar :: AVAR | eff) Unit
ticker out = forever (P.send' tickEvent out *> delay (Milliseconds 1000.0))

main :: forall eff. Eff (avar :: AVAR, console :: CONSOLE | eff) Unit
main = pure unit <* (runAff (const $ pure unit) $ flow ticker (log <<< show) (AppState 0))
