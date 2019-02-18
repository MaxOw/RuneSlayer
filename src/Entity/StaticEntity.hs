module Entity.StaticEntity
    ( StaticEntity, staticEntityToEntity

    , makeStaticEntity
    , testStaticEntityType_tree
    ) where

import Delude

import Types.Entity.StaticEntity
import Types.Entity.Appearance
import Entity.Utils
import Entity.Actions
import Types.Entity.ZIndex
import qualified Resource

--------------------------------------------------------------------------------

actOn :: StaticEntity -> EntityAction -> StaticEntity
actOn x _ = x

update
    :: StaticEntity
    -> EntityContext
    -> (Maybe StaticEntity, [DirectedEntityAction])
update x _ = (Just x, [])

render :: StaticEntity -> RenderContext -> RenderAction
render x ctx = withZIndex x $ locate x $ renderAppearance ctx
    $ x^.entityType.appearance

oracle :: StaticEntity -> EntityOracle
oracle x = def
   & location .~ Just (x^.location)
   & static   .~ True
   & zindex   .~ Just EntityZIndex_Vertical

--------------------------------------------------------------------------------

staticEntityToEntity :: StaticEntity -> Entity
staticEntityToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = oracle
   , makeSave   = EntitySum_StaticEntity
   , makeKind   = EntityKind_Static
   }

makeStaticEntity :: StaticEntityType -> StaticEntity
makeStaticEntity t = set entityType t def

testStaticEntityType_tree :: StaticEntityType
testStaticEntityType_tree = def
    & name       .~ "Tree"
    & appearance .~ app
    where
    app = Appearance_Compose
        [ Appearance_Sprite Resource.treeTrunk
        , Appearance_Translate (V2 0 2)
        $ Appearance_Sprite Resource.treeFoliage
        ]

