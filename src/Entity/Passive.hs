module Entity.Passive
    ( Passive, passiveToEntity
    , makePassive
    ) where

import Delude
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Hashable (hash)

import Entity
import Types.Entity.Passive
import Types.Debug
import Types.Equipment(EquipmentSlot)
import Engine.Common.Types (BBox(..))
import Entity.Utils
import Entity.Actions
import Types.Entity.Projectile
import ResourceManager (Resources, lookupPassive)
import Entity.Animation
    (AnimationProgression(..), makeStaticAnimation, renderAnimation)

import qualified Data.Collider as Collider

--------------------------------------------------------------------------------

actOn :: Passive -> EntityAction -> Passive
actOn x a = x & case a of
    EntityAction_AddItem    {} -> handleOnUpdate a
    EntityAction_RemoveItem {} -> handleOnUpdate a
    EntityAction_SelfPassTo {} -> handleOnUpdate a

    EntityAction_SelfFiredAsProjectile {} -> selfFireProjectile

    EntityAction_UseAction   {} -> handleOnUpdate a
    EntityAction_SetValue     v -> handleSetValue v
    EntityAction_RunAnimation k -> setAnimationKind k
    _ -> id

    where
    ks = x^.passiveType.passiveKind
    selfFireProjectile _
        | Set.member PassiveKind_Projectile ks = handleOnUpdate a x
        | otherwise = x

    handleSetValue ev = case ev of
        EntityValue_Location     v -> location     .~ Just v
        EntityValue_Direction    v -> direction    .~ Just v
        EntityValue_Animation    v -> animation    .~ v
        EntityValue_CenterOffset v -> centerOffset .~ v

    setAnimationKind k _ = x
        & animationState.current.kind .~ k
        & animationState.current.era  .~ 0
        & animationState.progression  .~ TransitionInto k (Stopped 1)

--------------------------------------------------------------------------------

update :: Passive -> EntityContext -> Q (Maybe Passive, [DirectedAction])
update x ctx = runUpdate x ctx $ do
    updateAnimationState
    allMatch _EntityAction_AddItem (containerAddItems . fmap fst)
    allMatch _EntityAction_RemoveItem containerRemoveItems
    firstMatch _EntityAction_SelfPassTo (uncurry selfPassTo)
    firstMatch _EntityAction_SelfFiredAsProjectile projectileFire
    mapM_ processAction =<< use (self.processOnUpdate)
    self.processOnUpdate .= mempty

selfPassTo :: Maybe EntityId -> Maybe EquipmentSlot -> Update Passive ()
selfPassTo mno mes = use (self.owner) >>= \mco -> when (mco /= mno) $ do
    removeSelfFromOwner
    case mno of
        Just no -> do
            addItem no
            self.location .= Nothing
        Nothing -> dropSelf
    self.owner .= mno
    where
    addItem x = useSelfId >>= \s -> addAction x $ EntityAction_AddItem s mes
    dropSelf = getRootLocation >>= \case
        Just loc -> do
            fct <- use $ context.frameCount
            rgid <- fromIntegral . hash <$> useSelfId
            let dloc = over _Wrapped (\v -> v + genDropOffset [fct + rgid]) loc
            self.location .= Just dloc
        Nothing -> selfDelete
            -- This sould probably never happen...
            -- Make this an assert?

removeSelfFromOwner :: Update Passive ()
removeSelfFromOwner = whenJustM (use $ self.owner) $ \oi ->
    addAction oi . EntityAction_RemoveItem =<< useSelfId

selfDelete :: Update Passive ()
selfDelete = do
    removeSelfFromOwner
    deleteSelf .= True

containerAddItems :: NonEmpty EntityId -> Update Passive ()
containerAddItems ls = do
    es <- catMaybes <$> mapM queryById (toList ls)
    mapM_ dropItem =<< fitIntoContainer es

containerRemoveItems :: NonEmpty EntityId -> Update Passive ()
containerRemoveItems rs = do
    cs <- self.content <%= (\x -> x List.\\ toList rs)
    es <- catMaybes <$> mapM queryById cs
    self.contentVolume .= sumOf (traverse.entity.oracleVolume.traverse) es

projectileFire :: (Location, V2D, EntityId, AttackPower) -> Update Passive ()
projectileFire (loc, vectorToTarget, tid, attackPower) = do
    selfDelete
    let maxDist   = distanceInMeters 12
    let projSpeed = speedInMetersPerSecond 40
    let vel = velocityFromSpeed vectorToTarget projSpeed
    projType <- use (self.passiveType)
    spawnProjectile $ Projectile
        { field_location     = loc
        , field_velocity     = vel
        , field_distanceLeft = maxDist
        , field_attackPower  = attackPower
        , field_target       = tid
        , field_passiveType  = projType
        }

processAction :: EntityAction -> Update Passive ()
processAction = \case
    EntityAction_UseAction n t -> performUseAction t n
    _ -> return ()
    where
    performUseAction t = mapM_ (performUseActionEffect t) <=< getUseAction
    getUseAction a = use $ self.passiveType.useActions.at(a).traverse

performUseActionEffect :: EntityId -> UseActionEffect -> Update Passive ()
performUseActionEffect t = \case
    UseActionEffect_TransformInto n -> transformInto n
    UseActionEffect_InspectContent  -> inspectContent
    UseActionEffect_DeleteSelf      -> selfDelete
    UseActionEffect_Heal          h -> heal h
    where
    transformInto n = do
        rs <- use $ context.resources
        case lookupPassive n rs of
            Just tt -> do
                self.passiveType .= tt
                self.animation   .= makeStaticAnimation
                    (renderAppearance rs $ tt^.appearance)
            Nothing -> selfDelete

    inspectContent = addWorldAction . WorldAction_InspectContent =<< useSelfId

    heal = addAction t . EntityAction_SelfHeal

fitIntoContainer :: [EntityWithId] -> Update Passive [EntityWithId]
fitIntoContainer ees = do
    alwd <- use (self.passiveType.containerType.traverse.allowKinds)
    let (allowed, otherItems) = splitAllowedItems alwd ees
    overflow <- go allowed
    return $ otherItems <> overflow
    where
    go [] = return []
    go (e:es) = do
        cv <- use (self.contentVolume)
        ct <- use (self.passiveType.containerType)
        let mv = fromMaybe 0 $ ct^?traverse.maxVolume
        if cv >= mv
        then return (e:es)
        else do
            let eid = e^.entityId
            self.content       %= (eid:)
            self.contentVolume += fromMaybe 0 (e^.entity.oracleVolume)
            go es

--------------------------------------------------------------------------------

render :: Passive -> RenderContext -> RenderAction
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
        -- , (DebugFlag_ShowCollisionShapes, renderCollisionShape cs)
        ]

    itemBBox = BBox (-0.5) (0.5)

oracle :: Passive -> EntityQuery a -> Maybe a
oracle x = \case
    EntityQuery_Location      -> x^.location
    EntityQuery_Name          -> Just showName
    EntityQuery_Volume        -> Just $ x^.passiveType.volume
    EntityQuery_FittingSlots  -> Just $ x^.passiveType.fittingSlots
    EntityQuery_PassiveType   -> Just $ x^.passiveType
    EntityQuery_Content       -> Just $ x^.content
    EntityQuery_MaxVolume     -> x^?passiveType.containerType.traverse.maxVolume
    EntityQuery_ItemAnimation -> x^.passiveType.animation
    EntityQuery_BehindBody    -> x^.passiveType.behindBody
    EntityQuery_Stats         -> Just $ x^.passiveType.stats
    EntityQuery_UseActions    -> Just $ Map.keys $ x^.passiveType.useActions
    EntityQuery_Owner         -> x^.owner
    _                         -> Nothing
    where
    nn = x^.passiveType.name._Wrapped
    ct = x^.content.to length
    showName = if x^?passiveType.containerType.traverse.showCount == Just True
        then nn <> " (" <> show ct <> ")"
        else nn

toKind :: Passive -> EntityKind
toKind x
    | isStopped = EntityKind_Passive
    | otherwise = EntityKind_Dynamic
    where
    isStopped = case x^.animationState.progression of
        Stopped _ -> True
        _         -> False

--------------------------------------------------------------------------------

passiveToEntity :: Passive -> Entity
passiveToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = oracle
   , makeSave   = Just . EntitySum_Passive
   , makeKind   = toKind
   }

makePassive :: Resources -> PassiveType -> Passive
makePassive rs t = def
    & passiveType  .~ t
    & animation .~ makeStaticAnimation (renderAppearance rs $ t^.appearance)
