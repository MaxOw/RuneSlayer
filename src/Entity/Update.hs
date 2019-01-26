module Entity.Update
    ( module Types.Entity.Update
    , runUpdate
    , liftUpdate
    ) where

import Delude
import Types.Entity
import Types.Entity.Update

--------------------------------------------------------------------------------

makeUpdateSt :: x -> EntityContext -> UpdateSt x
makeUpdateSt x ctx = UpdateSt
   { updateSt_self       = x
   , updateSt_context    = ctx
   , updateSt_deleteSelf = False
   , updateSt_actions    = def
   }

runUpdate
    :: x -> EntityContext -> Update x () -> (Maybe x, [DirectedEntityAction])
runUpdate x ctx act = (mx, result^..actions.traverse)
    where
    mx = if result^.deleteSelf then Nothing else Just (result^.self)
    result = execState act (makeUpdateSt x ctx)

liftUpdate :: (x -> x) -> Update x ()
liftUpdate f = self %= f
