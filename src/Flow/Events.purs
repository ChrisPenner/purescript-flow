module Flow.Events 
  ( class EventHandler
  , handle
  ) where

import Data.Variant (Variant)
class EventHandler state where
  handle :: forall evts
    . state -> Variant evts -> state
