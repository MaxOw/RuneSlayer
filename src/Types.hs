module Types
    ( module All

    , Game
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
type Game       a = Engine.Engine       St a
type EventHandler = Engine.EventHandler St
type Integrator   = Engine.Integrator   St
type Renderer     = Engine.Renderer     St

type Graphics   a = Game a

