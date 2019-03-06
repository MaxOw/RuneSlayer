module Entity.Container
    ( Container, containerToEntity
    , makeContainer

    , testContainerType_bag
    ) where

import Delude
import qualified Data.Set as Set

import Types.Equipment
import Types.Entity.Container
import Types.Entity.ItemType
import Types.Entity.Appearance
import Entity.Utils
import Entity.Actions
import Entity.Item
import qualified Resource

--------------------------------------------------------------------------------

actOn :: Container -> EntityAction -> Container
actOn x a = case a of
    EntityAction_AddItem  _ -> handleOnUpdate a x
    EntityAction_DropItem _ -> handleOnUpdate a x
    _ -> itemLikeActOn x a

update :: Container -> EntityContext -> Q (Maybe Container, [DirectedEntityAction])
update x ctx = runUpdate x ctx $ do
    whenMatch _EntityAction_AddItem containerAddItems
    mapM_ processAction =<< use (self.processOnUpdate)
    itemLikeUpdate

processAction :: EntityAction -> Update Container ()
processAction = \case
    EntityAction_DropItem i -> containerDropItem i
    _ -> return ()

render :: Container -> RenderContext -> RenderAction
render = itemLikeRender

thisOracle :: Container -> EntityOracle
thisOracle x = itemLikeOracle x
   & content      .~ Just (x^.content)
   & maxVolume    .~ Just (x^.containerType.maxVolume)

--------------------------------------------------------------------------------

containerToEntity :: Container -> Entity
containerToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = thisOracle
   , makeSave   = EntitySum_Container
   , makeKind   = EntityKind_Item
   }

makeContainer :: ContainerType -> Container
makeContainer x = set containerType x def

--------------------------------------------------------------------------------

testContainerType_bag :: ContainerType
testContainerType_bag = def
    & itemType.name           .~ "Bag"
    & itemType.volume         .~ volumeL 15
    & itemType.itemKind       .~ ItemKind_BigItem
    & itemType.appearance     .~ ap
    & itemType.fittingSlots   .~ Set.fromList [EquipmentSlot_Backpack]
    & maxVolume               .~ volumeL 15
    where
    ap = Appearance_Sprite Resource.bag

