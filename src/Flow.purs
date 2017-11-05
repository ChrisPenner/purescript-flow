module Flow
  ( flow
  ) where

import Prelude

import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Flow.Events (class EventHandler, handle)
import Pipes.Aff as P

flow :: forall appstate eff evts
 .  EventHandler appstate
 => (P.Output (Variant evts) -> Aff (avar :: AVAR | eff) Unit)
 -> (appstate -> Aff (avar :: AVAR | eff) Unit) 
 -> appstate ->  Aff (avar :: AVAR | eff) Unit
flow evtProducer renderer appstate = do
  Tuple inp out <- P.split <$> P.spawn P.unbounded
  _ <- forkAff $ evtProducer out
  flip tailRecM appstate \state -> do
    renderer state
    P.recv' inp >>= case _ of
                         Nothing -> pure $ Done unit
                         Just evt -> pure <<< Loop $ handle state evt  
