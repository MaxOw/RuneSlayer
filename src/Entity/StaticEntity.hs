module Entity.StaticEntity
    ( StaticEntity, staticEntityToEntity

    , makeStaticEntity
    , testStaticEntityType_tree
    ) where

import Delude
import qualified Data.Set as Set

import Types.Debug
import Types.Entity.StaticEntity
import Types.Entity.Appearance
import Entity.Utils
import Entity.Actions
import Types.Entity.ZIndex
import qualified Resource
import qualified Data.Collider as Collider
import qualified Data.Collider.Types as Collider

import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color

--------------------------------------------------------------------------------

actOn :: StaticEntity -> EntityAction -> StaticEntity
actOn x _ = x

update
    :: StaticEntity
    -> EntityContext
    -> Q (Maybe StaticEntity, [DirectedEntityAction])
update x _ = return (Just x, [])

render :: StaticEntity -> RenderContext -> RenderAction
render x ctx = withZIndex x $ locate x $ renderComposition
    [ renderDebug
    , renderAppearance ctx $ x^.entityType.appearance ]
    where
    renderDebug
        = renderComposition $ map snd
        $ filter (\(f, _) -> Set.member f $ ctx^.debugFlags)
        [ (DebugFlag_ShowCollisionShapes, renderCollisionShapes)
        ]

    renderCollisionShapes = monoidJust (x^.entityType.collisionShape) $ \case
        Collider.Circle d -> renderShape $ def
            & shapeType .~ SimpleCircle
            & color     .~ Color.withOpacity Color.red 0.3
            & scale     (d^.Collider.radius)
            & translate (d^.Collider.center)
            & zindex    .~ 10000

oracle :: StaticEntity -> EntityOracle
oracle x = def
   & location       .~ Just (x^.location)
   & zindex         .~ Just EntityZIndex_Vertical
   & collisionShape .~ (x^.entityType.collisionShape)

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
    & collisionShape .~ Just (Collider.circle (V2 0 (-0.2)) 0.6)
    where
    app = Appearance_Compose
        [ Appearance_Sprite Resource.treeTrunk
        , Appearance_Translate (V2 0 2)
        $ Appearance_Sprite Resource.treeFoliage
        ]

