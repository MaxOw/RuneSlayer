module Entity
    ( Entity (..), EntityParts
    , makeEntity
    ) where

import Delude
import Types.Entity

makeEntity :: EntityParts p -> p -> Entity
makeEntity me = f
    where
    f p = Entity
        { entityActOn  = f . makeActOn me p
        , entityUpdate = over _1 (fmap f) . makeUpdate me p
        , entityRender = makeRender me p
        , entityOracle = makeOracle me p
        , entitySave   = makeSave me p
        , entityKind   = makeKind me
        }

