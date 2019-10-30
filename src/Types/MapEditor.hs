module Types.MapEditor where

import Delude
import Engine (RenderAction)
import Types.DirectedAction (SpawnEntity)
import Data.Zipper (Zipper)
import Types.Entity.Common (Location)
import Types.Entity.PassiveType (PassiveTypeName)
import Types.Entity.Agent (AgentTypeName)

data MapEditorAction
   = MapEditorAction_PlaceEntity
   | MapEditorAction_NextCategory
   | MapEditorAction_PrevCategory
   | MapEditorAction_SelectNext
   | MapEditorAction_SelectPrev
   deriving (Eq, Ord, Show, Generic)

data SelectorCategory
   = SelectorCategory_Passives
   | SelectorCategory_Agents
   deriving (Generic, Eq, Ord, Enum, Bounded)
instance Default SelectorCategory where def = SelectorCategory_Passives

data SelectedEntity = SelectedEntity
   { field_spawnEntity  :: SpawnEntity
   , field_renderAction :: RenderAction
   } deriving (Generic)

data MapEditorState = MapEditorState
   { field_selectedEntity   :: Maybe SelectedEntity
   , field_passives         :: Zipper PassiveTypeName
   , field_agents           :: Zipper AgentTypeName
   , field_selectorCategory :: SelectorCategory
   , field_placedList       :: [(SpawnEntity, Location)]
   } deriving (Generic)
instance Default MapEditorState

passives :: Lens' MapEditorState (Zipper PassiveTypeName)
passives = ff#passives

agents :: Lens' MapEditorState (Zipper AgentTypeName)
agents = ff#agents

selectorCategory :: Lens' MapEditorState SelectorCategory
selectorCategory = ff#selectorCategory

placedList :: Lens' MapEditorState [(SpawnEntity, Location)]
placedList = ff#placedList
