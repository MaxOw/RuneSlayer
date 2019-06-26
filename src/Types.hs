module Types
    ( module All

    , Event
    , EventHandler
    , Integrator
    , Renderer
    , Graphics
    ) where

-- import Delude
import qualified Engine

import Types.St as All

type Event        = Engine.Event
type EventHandler = Engine.EventHandler St
type Integrator   = Engine.Integrator   St
type Renderer     = Engine.Renderer     St

type Graphics   a = Game a

