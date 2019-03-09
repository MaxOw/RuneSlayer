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
    :: x -> EntityContext -> Update x () -> Q (Maybe x, [DirectedAction])
runUpdate x ctx act = do
    result <- execStateT act (makeUpdateSt x ctx)
    let mx = if result^.deleteSelf then Nothing else Just (result^.self)
    return (mx, result^..actions.traverse)

liftUpdate :: (x -> x) -> Update x ()
liftUpdate f = self %= f
