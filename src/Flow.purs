module Flow
  ( flow
  , EventProducer
  ) where

import Prelude

import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM)
import Data.Maybe (maybe)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Pipes.Aff as P

type EventProducer eff evts = P.Output (Variant evts) -> Aff (avar :: AVAR | eff) Unit

flow :: forall appstate eff evts
  . (appstate -> Variant evts -> appstate)
 -> Array (EventProducer eff evts)
 -> (appstate -> Aff (avar :: AVAR | eff) Unit)
 -> appstate
 -> Aff (avar :: AVAR | eff) Unit
flow reducer evtProducers renderer appstate = do
  Tuple inp out <- P.split <$> P.spawn P.unbounded
  traverse_ (\prod -> forkAff (prod out)) evtProducers
  flip tailRecM appstate \state -> do
    renderer state
    evt <- P.recv' inp 
    pure $ maybe (Done unit) (Loop <<< reducer state) evt
