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

import Types.Equipment
import Types.Entity.Item
import Types.Entity.ItemType
import Entity.Utils
import Entity.Actions

import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color

--------------------------------------------------------------------------------

itemLikeActOn
    :: HasLocation        s (Maybe Location)
    => HasOwner           s (Maybe EntityId)
    => HasProcessOnUpdate s [EntityAction]
    => s -> EntityAction -> s
itemLikeActOn x a = case a of
    EntityAction_PickItemBy eid -> pickItemBy eid
    EntityAction_DropItemAt loc -> dropItemAt loc
    _ -> x

    where
    pickItemBy eid = case x^.owner of
        Just _  -> x
        Nothing -> x
            & location .~ Nothing
            & owner    .~ (Just eid)
            & handleOnUpdate a

    dropItemAt loc = x
        & location .~ Just loc
        & owner    .~ Nothing

itemLikeUpdatePure
    :: HasProcessOnUpdate s [EntityAction]
    => HasOwner           s (Maybe EntityId)
    => s -> EntityContext -> (Maybe s, [DirectedEntityAction])
itemLikeUpdatePure x ctx = runUpdate x ctx $ do
    itemLikeUpdate

itemLikeUpdate
    :: HasProcessOnUpdate s [EntityAction]
    => HasOwner           s (Maybe EntityId)
    => Update s ()
itemLikeUpdate = do
    anyMatch _EntityAction_PickItemBy pickUpInformOwner
    self.processOnUpdate .= mempty

itemLikeRender
    :: HasLocation x (Maybe Location)
    => HasItemType x ItemType
    => x -> RenderAction
itemLikeRender x = ifJustLocation x $ maybeLocate x $ case x^.itemType.appearance of
    Appearance_SimpleCircle s c -> renderCircle s c
    Appearance_SimpleSquare s c -> renderSquare s c
    where
    renderCircle = renderS SimpleCircle
    renderSquare = renderS SimpleSquare
    renderS t s c = scale s $ renderShape $ def
        & shapeType .~ t
        & color     .~ c

itemLikeOracle
    :: HasLocation s (Maybe Location)
    => HasItemType s ItemType
    => s -> EntityOracle
itemLikeOracle x = def
   & location     .~ (x^.location)
   & name         .~ Just (x^.itemType.name)
   & itemKind     .~ Just (x^.itemType.itemKind)
   & fittingSlots .~ (x^.itemType.fittingSlots)

--------------------------------------------------------------------------------

itemToEntity :: Item -> Entity
itemToEntity = makeEntity $ EntityParts
   { makeActOn  = itemLikeActOn
   , makeUpdate = itemLikeUpdatePure
   , makeRender = itemLikeRender
   , makeOracle = itemLikeOracle
   , makeSave   = EntityItem
   }

makeItem :: ItemType -> Item
makeItem t = set itemType t def

--------------------------------------------------------------------------------

testItemType_helmet :: ItemType
testItemType_helmet = def
    & name         .~ "Helmet"
    & volume       .~ volumeL 1.5
    & itemKind     .~ ItemKind_BigItem
    & appearance   .~ Appearance_SimpleCircle 0.3 (Color.opaque Color.gray)
    & fittingSlots .~ Set.fromList [EquipmentSlot_Head]

testItemType_healthPotion :: ItemType
testItemType_healthPotion = def
    & name         .~ "Health Potion"
    & volume       .~ volumeL 0.1
    & itemKind     .~ ItemKind_SmallItem
    & appearance   .~ Appearance_SimpleCircle 0.1 (Color.opaque Color.red)
    & fittingSlots .~ Set.fromList []

