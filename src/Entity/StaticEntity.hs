module Entity.StaticEntity
    ( StaticEntity, staticEntityToEntity

    , makeStaticEntity
    ) where

import Delude
import qualified Data.Set as Set

import Types.Debug
import Types.Entity.StaticEntity
import Types.Entity.Appearance
import Entity.Utils
import Entity.Actions
import Types.Entity.ZIndex
import Types.Sprite
import qualified Data.Collider as Collider

--------------------------------------------------------------------------------

actOn :: StaticEntity -> EntityAction -> StaticEntity
actOn x _ = x

update
    :: StaticEntity
    -> EntityContext
    -> Q (Maybe StaticEntity, [DirectedAction])
update x _ = return (Just x, [])

render :: StaticEntity -> RenderContext -> RenderAction
render x ctx = withZIndex x $ locate x $ renderComposition
    [ renderDebug
    , renderAppearance ctx $ x^.entityType.appearance ]
    where
    renderDebug
        = renderComposition $ map snd
        $ filter (\(f, _) -> Set.member f $ ctx^.debugFlags)
        [ -- (DebugFlag_ShowCollisionShapes, renderCollisionShape cs)
        ]

    -- cs = x^.entityType.collisionShape

oracle :: StaticEntity -> EntityQuery a -> Maybe a
oracle x = \case
    EntityQuery_Location -> Just $ x^.location
    EntityQuery_Zindex   -> Just EntityZIndex_Vertical
    _                    -> Nothing

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

    -- & collisionShape .~ Just (
        -- translateY (-0.8) $ Collider.circle 0 0.3)
    -- & collisionShape .~ Just (Collider.circle (P $ V2 0 (-0.3)) 0.3)
