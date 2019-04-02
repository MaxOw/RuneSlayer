module Types.Entity.Update where

import Delude
import Types.Entity

--------------------------------------------------------------------------------

data UpdateSt x = UpdateSt
   { field_self       :: x
   , field_context    :: EntityContext
   , field_deleteSelf :: Bool
   , field_actions    :: Seq DirectedAction
   } deriving (Generic)


type Update x = StateT (UpdateSt x) Q

