module Entity.Item
    ( Item, itemToEntity
    , makeItem

    , itemLikeActOn
    , itemLikeUpdate
    , itemLikeRender
    , itemLikeOracle

    , testItemType_helmet
    , testItemType_healthPotion
    ) where

import Delude
import qualified Data.Set as Set

-- import Engine (HasTextureId(..))
import Types.Equipment
import Types.Entity.Item
import Types.Entity.ItemType
import Types.Entity.Appearance
import Types.Debug
import Engine.Common.Types (BBox(..))
import Entity.Utils
import Entity.Actions
import qualified Resource

--------------------------------------------------------------------------------

itemLikeActOn
    :: HasLocation        s (Maybe Location)
    => HasOwner           s (Maybe EntityId)
    => HasProcessOnUpdate s [EntityAction]
    => s -> EntityAction -> s
itemLikeActOn x a = case a of
    EntityAction_SelfPassedTo  eid -> selfPassedTo eid
    EntityAction_SelfAddedBy   eid -> selfAddedBy eid
    EntityAction_SelfDroppedAt loc -> slefDroppedAt loc
    _ -> x

    where
    selfAddedBy eid = case x^.owner of
        Just _  -> x
        Nothing -> x
            & location .~ Nothing
            & owner    .~ (Just eid)
            & handleOnUpdate a

    selfPassedTo eid = x
            & location .~ Nothing
            & owner    .~ (Just eid)

    slefDroppedAt loc = x
        & location .~ Just loc
        & owner    .~ Nothing

itemLikeUpdatePure
    :: HasProcessOnUpdate s [EntityAction]
    => HasOwner           s (Maybe EntityId)
    => s -> EntityContext -> Q (Maybe s, [DirectedAction])
itemLikeUpdatePure x ctx = runUpdate x ctx $ do
    itemLikeUpdate

itemLikeUpdate
    :: HasProcessOnUpdate s [EntityAction]
    => HasOwner           s (Maybe EntityId)
    => Update s ()
itemLikeUpdate = do
    whenMatch _EntityAction_SelfAddedBy pickUpInformOwner
    self.processOnUpdate .= mempty

itemLikeRender
    :: HasLocation x (Maybe Location)
    => HasItemType x ItemType
    => GetZIndex   x Word32
    => x -> RenderContext -> RenderAction
itemLikeRender x ctx = ifJustLocation x $ maybeLocate x $ withZIndex x
    $ renderComposition
    [ itemRenderAction
    , renderDebug
    ]
    where
    itemRenderAction = renderAppearance ctx $ x^.itemType.appearance

    renderDebug
        = renderComposition $ map snd
        $ filter (\(f, _) -> Set.member f $ ctx^.debugFlags)
        [ (DebugFlag_ShowDynamicBoundingBoxes, renderBBox itemBBox)
        ]

    itemBBox = BBox (-0.5) (0.5)

itemLikeOracle
    :: HasLocation s (Maybe Location)
    => HasItemType s ItemType
    => s -> EntityOracle
itemLikeOracle x = def
   & location     .~ (x^.location)
   & name         .~ Just (x^.itemType.name)
   & volume       .~ Just (x^.itemType.volume)
   & itemKind     .~ Just (x^.itemType.itemKind)
   & fittingSlots .~ (x^.itemType.fittingSlots)

--------------------------------------------------------------------------------

itemToEntity :: Item -> Entity
itemToEntity = makeEntity $ EntityParts
   { makeActOn  = itemLikeActOn
   , makeUpdate = itemLikeUpdatePure
   , makeRender = itemLikeRender
   , makeOracle = itemLikeOracle
   , makeSave   = EntitySum_Item
   , makeKind   = EntityKind_Item
   }

makeItem :: ItemType -> Item
makeItem t = set itemType t def

--------------------------------------------------------------------------------

testItemType_helmet :: ItemType
testItemType_helmet = def
    & name         .~ "Helmet"
    & volume       .~ volumeL 1.5
    & itemKind     .~ ItemKind_BigItem
    & appearance   .~ Appearance_Sprite Resource.helmet
    & fittingSlots .~ Set.fromList [EquipmentSlot_Head]

testItemType_healthPotion :: ItemType
testItemType_healthPotion = def
    & name         .~ "Health Potion"
    & volume       .~ volumeL 0.1
    & itemKind     .~ ItemKind_SmallItem
    & appearance   .~ Appearance_Sprite Resource.healthPotion
    & fittingSlots .~ Set.fromList []

