module Entity.Item
    ( Item, itemToEntity
    , makeItem
    ) where

import Delude
import qualified Data.Set as Set
import qualified Data.List as List

import Types.Entity (oracle)
import Types.Entity.Item
import Types.Entity.ItemType
import Types.Debug
import Engine.Common.Types (BBox(..))
import Entity.Utils
import Entity.Actions

--------------------------------------------------------------------------------

actOn :: Item -> EntityAction -> Item
actOn x a = case a of
    -- For containers
    EntityAction_AddItem  _ -> handleOnUpdate a x
    EntityAction_DropItem _ -> handleOnUpdate a x
    -- For any item
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

--------------------------------------------------------------------------------

update :: Item -> EntityContext -> Q (Maybe Item, [DirectedAction])
update x ctx = runUpdate x ctx $ do
    whenMatch _EntityAction_SelfAddedBy pickUpInformOwner
    whenMatch _EntityAction_AddItem containerAddItems
    mapM_ processAction =<< use (self.processOnUpdate)
    self.processOnUpdate .= mempty

processAction :: EntityAction -> Update Item ()
processAction = \case
    EntityAction_DropItem i -> containerDropItem i
    _ -> return ()

containerAddItems :: Update Item ()
containerAddItems = do
    os <- fitIntoContainer =<< getItemsToAdd
    mapM_ containerDropItem os

containerDropItem :: HasEntityId i EntityId => i -> Update Item ()
containerDropItem e = use (self.location) >>= \case
    Nothing -> use (self.owner) >>= \mo -> whenJust mo $ \o -> do
        addAction o $ EntityAction_OwnerDropItem eid
        self.content %= List.delete eid
    Just lc -> use (context.frameCount) >>= \fct -> do
        addAction eid $ makeDropItem fct lc eid
        self.content %= List.delete eid
    where
    eid = e^.entityId

fitIntoContainer :: [EntityWithId] -> Update Item [EntityWithId]
fitIntoContainer ees = do
    let (smallItems, otherItems) = splitItemKind ItemKind_SmallItem ees
    overflow <- go smallItems
    return $ otherItems <> overflow
    where
    go [] = return []
    go (e:es) = do
        cv <- use (self.contentVolume)
        ct <- use (self.itemType.containerType)
        let mv = fromMaybe 0 $ ct^?traverse.maxVolume
        if cv >= mv
        then return (e:es)
        else do
            let eid = e^.entityId
            self.content       %= (eid:)
            self.contentVolume += fromMaybe 0 (e^.entity.oracle.volume)
            addAction eid . EntityAction_SelfPassedTo =<< useSelfId
            go es

--------------------------------------------------------------------------------

render :: Item -> RenderContext -> RenderAction
render x ctx = ifJustLocation x $ maybeLocate x $ withZIndex x
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

thisOracle :: Item -> EntityOracle
thisOracle x = def
   & location     .~ (x^.location)
   & name         .~ Just (x^.itemType.name._Wrapped)
   & volume       .~ Just (x^.itemType.volume)
   & itemKind     .~ Just (x^.itemType.itemKind)
   & fittingSlots .~ (x^.itemType.fittingSlots)
   & content      .~ Just (x^.content)
   & maxVolume    .~ (x^?itemType.containerType.traverse.maxVolume)

--------------------------------------------------------------------------------

itemToEntity :: Item -> Entity
itemToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = thisOracle
   , makeSave   = EntitySum_Item
   , makeKind   = EntityKind_Item
   }

makeItem :: ItemType -> Item
makeItem t = set itemType t def

