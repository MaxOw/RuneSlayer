{-# Language TemplateHaskell #-}
module Types.Entity.Update where

import Delude
import Types.Entity

--------------------------------------------------------------------------------

data UpdateSt x = UpdateSt
   { updateSt_self       :: x
   , updateSt_context    :: EntityContext
   , updateSt_deleteSelf :: Bool
   , updateSt_actions    :: Seq DirectedAction
   }
makeFieldsCustom ''UpdateSt

type Update x = StateT (UpdateSt x) Q

