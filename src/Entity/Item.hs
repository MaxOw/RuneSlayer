module Entity.Item
    ( Item, itemToEntity
    , makeItem
    ) where

import Delude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.List as List

import Entity
import Types.Entity.Item
import Types.Debug
import Engine.Common.Types (BBox(..))
import Entity.Utils
import Entity.Actions
import Types.Entity.Projectile
import ResourceManager (Resources)
import Entity.Animation
    (AnimationProgression(..), makeStaticAnimation, renderAnimation)

--------------------------------------------------------------------------------

actOn :: Item -> EntityAction -> Item
actOn x a = x & case a of
    -- For containers
    EntityAction_AddItem  {} -> handleOnUpdate a
    EntityAction_DropItem {} -> handleOnUpdate a
    EntityAction_PassItem {} -> handleOnUpdate a
    -- For any item
    EntityAction_SelfPassedTo  eid -> selfPassedTo eid
    EntityAction_SelfAddedBy   eid -> selfAddedBy eid
    EntityAction_SelfDroppedAt loc -> slefDroppedAt loc
    EntityAction_SelfFiredAsProjectile {} -> selfFireProjectile
    EntityAction_SelfUseOn   {} -> handleOnUpdate a
    EntityAction_SetValue     v -> handleSetValue v
    EntityAction_RunAnimation k -> setAnimationKind k
    _ -> id

    where
    selfPassedTo eid _ = x
            & location .~ Nothing
            & owner    .~ (Just eid)

    selfAddedBy eid _ = case x^.owner of
        Just _  -> x
        Nothing -> x
            & location .~ Nothing
            & owner    .~ (Just eid)
            & handleOnUpdate a

    slefDroppedAt loc _ = x
        & location .~ Just loc
        & owner    .~ Nothing

    ks = x^.itemType.itemKind
    selfFireProjectile _
        | Set.member ItemKind_Projectile ks = handleOnUpdate a x
        | otherwise = x

    handleSetValue ev = case ev of
        EntityValue_Location     v -> location  .~ Just v
        EntityValue_Direction    v -> direction .~ Just v
        EntityValue_Animation    v -> animation .~ v
        EntityValue_CenterOffset v -> centerOffset .~ v

    setAnimationKind k _ = x
        & animationState.current.kind .~ k
        & animationState.current.era  .~ 0
        & animationState.progression  .~ TransitionInto k (Stopped 1)

--------------------------------------------------------------------------------

update :: Item -> EntityContext -> Q (Maybe Item, [DirectedAction])
update x ctx = runUpdate x ctx $ do
    updateAnimationState
    whenMatch _EntityAction_SelfAddedBy pickUpInformOwner
    whenMatch _EntityAction_AddItem containerAddItems
    firstMatch _EntityAction_SelfFiredAsProjectile projectileFire
    firstMatch _EntityAction_SelfUseOn selfUseOn
    mapM_ processAction =<< use (self.processOnUpdate)
    self.processOnUpdate .= mempty

projectileFire :: (Location, V2D, EntityId, AttackPower) -> Update Item ()
projectileFire (loc, vectorToTarget, tid, attackPower) = do
    deleteSelf .= True
    let maxDist   = distanceInMeters 12
    let projSpeed = speedInMetersPerSecond 40
    let vel = velocityFromSpeed vectorToTarget projSpeed
    projType <- use (self.itemType)
    spawnProjectile $ Projectile
        { field_location     = loc
        , field_velocity     = vel
        , field_distanceLeft = maxDist
        , field_attackPower  = attackPower
        , field_target       = tid
        , field_itemType     = projType
        }

selfUseOn :: EntityId -> Update Item ()
selfUseOn t = mapM_ performUseEffect =<< use (self.itemType.useEffects)
    where
    performUseEffect = \case
        ItemUseEffect_TransformInto n -> transformInto n
        ItemUseEffect_Heal h -> addAction t $ EntityAction_SelfHeal h

    transformInto n = do
        rs <- use $ context.resources
        case (HashMap.lookup n $ rs^.itemsMap) of
            Just tt -> do
                self.itemType  .= tt
                self.animation .= makeStaticAnimation
                    (renderAppearance rs $ tt^.appearance)
            Nothing -> deleteSelf .= True
        -- TODO:
        -- There should be also be proper handling for specific kinds of
        -- transformations, like for example if we where to change a
        -- container into a non container we should handle its contents
        -- somehow.
        -- For example drop all items:
        -- > mapM_ containerDropItem =<< use (self.content)
        -- Or we could just add that as a standalone ItemUseEffect and
        -- use it in ItemTypes.dhall when needed.

processAction :: EntityAction -> Update Item ()
processAction = \case
    EntityAction_DropItem i   -> containerDropItem i
    EntityAction_PassItem i t -> containerPassItem i t
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

containerPassItem :: EntityId -> EntityId -> Update Item ()
containerPassItem itemId targetId = do
    addAction itemId $ EntityAction_SelfPassedTo targetId
    self.content %= List.delete itemId

fitIntoContainer :: [EntityWithId] -> Update Item [EntityWithId]
fitIntoContainer ees = do
    alwd <- use (self.itemType.containerType.traverse.ff#allowKinds)
    let (smallItems, otherItems) = splitAllowedItems alwd ees
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
            self.contentVolume += fromMaybe 0 (e^.entity.oracleVolume)
            addAction eid . EntityAction_SelfPassedTo =<< useSelfId
            go es

--------------------------------------------------------------------------------

render :: Item -> RenderContext -> RenderAction
render x ctx = ifJustLocation x $ maybeLocate x $ withZIndex x
    $ translate (x^.centerOffset) $ renderComposition
    [ itemRenderAction
    , renderDebug
    ]
    where
    itemRenderAction = renderAnimation (x^.animationState) (x^.animation)

    renderDebug
        = renderComposition $ map snd
        $ filter (\(f, _) -> Set.member f $ ctx^.debugFlags)
        [ (DebugFlag_ShowDynamicBoundingBoxes, renderBBox itemBBox)
        ]

    itemBBox = BBox (-0.5) (0.5)

oracle :: Item -> EntityQuery a -> Maybe a
oracle x = \case
    EntityQuery_Location      -> x^.location
    EntityQuery_Name          -> Just showName
    EntityQuery_Volume        -> Just $ x^.itemType.volume
    EntityQuery_FittingSlots  -> Just $ x^.itemType.fittingSlots
    EntityQuery_ItemType      -> Just $ x^.itemType
    EntityQuery_Content       -> Just $ x^.content
    EntityQuery_MaxVolume     -> x^?itemType.containerType.traverse.maxVolume
    EntityQuery_ItemAnimation -> x^.itemType.animation
    EntityQuery_Stats         -> Just $ x^.itemType.stats
    _                         -> Nothing
    where
    nn = x^.itemType.name._Wrapped
    ct = x^.content.to length
    showName = if x^?itemType.containerType.traverse.showCount == Just True
        then nn <> " (" <> show ct <> ")"
        else nn

toKind :: Item -> EntityKind
toKind x
    | isStopped = EntityKind_Item
    | otherwise = EntityKind_Dynamic
    where
    isStopped = case x^.animationState.progression of
        Stopped _ -> True
        _         -> False

--------------------------------------------------------------------------------

itemToEntity :: Item -> Entity
itemToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = oracle
   , makeSave   = Just . EntitySum_Item
   , makeKind   = toKind
   }

makeItem :: Resources -> ItemType -> Item
makeItem rs t = def
    & itemType  .~ t
    & animation .~ makeStaticAnimation (renderAppearance rs $ t^.appearance)

--------------------------------------------------------------------------------

stats :: Lens' ItemType Stats
stats = ff#stats

useEffects :: Lens' ItemType [ItemUseEffect]
useEffects = ff#useEffects

showCount :: Lens' ContainerType Bool
showCount = ff#showCount

centerOffset :: Lens' Item V2D
centerOffset = ff#centerOffset
