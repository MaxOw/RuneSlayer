module Entity.Player
    ( Player, playerToEntity
    ) where

import Delude
import qualified Data.Set as Set

import Types.Debug
import Types.Entity
import Types.Entity.Player
import Entity.Utils
import Entity.Actions

import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color

import qualified Resource

import qualified Entity.Animation as Animation

--------------------------------------------------------------------------------

actOn :: Player -> EntityAction -> Player
actOn x a = case a of
    EntityAction_ToggleDebug       f -> toggleDebugFlag  f x
    EntityAction_DebugRunAnimation k -> setAnimationKind k x
    EntityAction_SetMoveVector     v -> setMoveVector    v x
    EntityAction_DropAllItems        -> handleOnUpdate   a x
    EntityAction_AddItem           _ -> handleOnUpdate   a x
    EntityAction_DropItem          _ -> handleOnUpdate   a x
    EntityAction_OwnerDropItem     _ -> handleOnUpdate   a x
    _ -> x
    where
    setAnimationKind k x = x & animation.kind .~ k

update :: Player -> EntityContext -> Q (Maybe Player, [DirectedEntityAction])
update x ctx = runUpdate x ctx $ do
    integrateLocation
    separateCollision
    updateAnimation
    anyMatch _EntityAction_AddItem  addItems
    anyMatch _EntityAction_DropAllItems dropAllItems
    mapM_ processAction =<< use (self.processOnUpdate)
    self.processOnUpdate .= mempty

processAction :: EntityAction -> Update Player ()
processAction = \case
    EntityAction_DropItem i -> dropItem i
    EntityAction_OwnerDropItem i -> dropItemAction i
    _ -> return ()

--------------------------------------------------------------------------------

render :: Player -> RenderContext -> RenderAction
render x ctx = withZIndex x $ locate x $ renderComposition
    [ renderDebug
    -- , renderShape shape & scale 0.3
    , renderAnimaiton
        [ Resource.maleBody
        , Resource.malePants
        , Resource.maleShirt
        , Resource.maleHair
        ]
    ]
    where
    renderDebug = renderComposition $ localDebug <> globalDebug

    renderAnimaiton = translateY 0.8 . renderComposition . map renderAnimPart
    renderAnimPart  = renderSprite ctx . Animation.selectPart (x^.animation)

    localDebug = map snd
        $ filter (\(f, _) -> x^.debugFlags.f)
        [ (drawPickupRange, renderPickupRange)
        ]

    globalDebug = map snd
        $ filter (\(f, _) -> Set.member f $ ctx^.debugFlags)
        [ (DebugFlag_ShowCollisionShapes, renderCollisionShape cs)
        ]

    cs = x^.collisionShape

    {-
    shape = def
        & shapeType   .~ SimpleCircle
        & color       .~ Color.opaque Color.blue
    -}

    rangeScale = defaultPickupRange^._Wrapped
    renderPickupRange = scale rangeScale $ renderShape $ def
        & shapeType   .~ SimpleCircle
        & color       .~ Color.withOpacity Color.red 0.3

thisOracle :: Player -> EntityOracle
thisOracle x = def
   & location       .~ Just (x^.location)
   & equipment      .~ Just (x^.equipment)
   & collisionShape .~ (locate x <$> x^.collisionShape)

--------------------------------------------------------------------------------

playerToEntity :: Player -> Entity
playerToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = thisOracle
   , makeSave   = EntitySum_Player
   , makeKind   = EntityKind_Dynamic
   }
