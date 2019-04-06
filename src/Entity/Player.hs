module Entity.Player
    ( Player, playerToEntity
    , makePlayer
    ) where

import Delude
import qualified Data.Set as Set
import qualified Diagrams.TwoD.Transform as T

import Types.Debug
import Types.Entity
import Types.Entity.Player
import Entity.Utils
import Entity.Actions
import ResourceManager (Resources, lookupAnimation)

import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color

import qualified Entity.Animation as Animation

--------------------------------------------------------------------------------

actOn :: Player -> EntityAction -> Player
actOn x a = x & case a of
    EntityAction_ToggleDebug       f -> toggleDebugFlag  f
    EntityAction_DebugRunAnimation k -> setAnimationKind k
    EntityAction_SetMoveVector     v -> setMoveVector    v
    EntityAction_DropAllItems        -> handleOnUpdate   a
    EntityAction_AddItem           _ -> handleOnUpdate   a
    EntityAction_DropItem          _ -> handleOnUpdate   a
    EntityAction_OwnerDropItem     _ -> handleOnUpdate   a
    EntityAction_ExecuteAttack       -> handleOnUpdate   a
    EntityAction_SelfAttacked      _ -> handleOnUpdate   a
    _ -> id
    where
    setAnimationKind k _ = x
        & animationState.current.kind .~ k
        & animationState.current.era  .~ 0
        & animationState.progression  .~ Animation.defaultTransition

update :: Player -> EntityContext -> Q (Maybe Player, [DirectedAction])
update x ctx = runUpdate x ctx $ do
    whenMatch _EntityAction_ExecuteAttack executeAttack
    updateAnimation
    updateEffects defaultDelta
    playerIntegrateLocation
    separateCollision
    autoTarget
    whenMatch _EntityAction_AddItem      addItems
    whenMatch _EntityAction_DropAllItems dropAllItems
    anyMatch  _EntityAction_SelfAttacked procAttacked
    mapM_ processAction =<< use (self.processOnUpdate)
    self.processOnUpdate .= mempty

procAttacked :: NonEmpty AttackPower -> Update Player ()
procAttacked as = do
    ds <- mapM applyDefence as
    mapM_ (addEffect . Animation.HitEffect) ds
    where
    applyDefence     x = return x

executeAttack :: Update Player ()
executeAttack = do
    mt <- fmap join . mapM queryById =<< use (self.target)
    whenJust mt $ \te -> do
        let mloc = te^.entity.oracle.location
        whenJust mloc $ \loc -> do
            sloc <- use $ self.location
            executeAttackAt te $ loc^._Wrapped - sloc^._Wrapped

executeAttackAt :: EntityWithId -> V2 Float -> Update Player ()
executeAttackAt targetEntity vectorToTarget = do
    -- attackRange <- use $ self.ff#attackRange
    let attackRange = Distance 2
    dist <- distanceToEntity attackRange targetEntity
    when (dist < attackRange) $ do
        -- TODO: Choose attack animation based on weapon
        self.animationState.current.direction %= Animation.vecToDir vectorToTarget
        self.animationState.current.kind .= Animation.Slash
        self.animationState.current.era  .= 0
        self.animationState.progression  .= Animation.defaultTransition

        let attackPower = AttackPower 1 -- TODO: Calculate attack power
        addAction targetEntity $ EntityAction_SelfAttacked attackPower

playerIntegrateLocation :: Update Player ()
playerIntegrateLocation = do
    k <- use $ self.animationState.current.kind
    when (k == Animation.Walk) integrateLocation

autoTarget :: Update Player ()
autoTarget = do
    ds <- queryInRadius EntityKind_Dynamic (disM 8)
    sid <- use $ context.selfId
    loc <- use $ self.location
    let hs = sortWith (distanceTo loc) $ filter ((/=sid) . view entityId) ds
    let newTarget = viaNonEmpty head (map (view entityId) hs)
    currentTarget <- use $ self.target
    self.target .= viaNonEmpty head (map (view entityId) hs)
    when (newTarget /= currentTarget) $ do
        mapM_ (flip addAction EntityAction_SelfUnmarkAsTarget) currentTarget
        mapM_ (flip addAction EntityAction_SelfMarkAsTarget) newTarget

    where
    distanceTo loc e = fromMaybe 10000 $
        (distance (loc^._Wrapped) . view _Wrapped <$> e^.entity.oracle.location)

processAction :: EntityAction -> Update Player ()
processAction = \case
    EntityAction_DropItem i -> dropItem i
    EntityAction_OwnerDropItem i -> dropItemAction i
    _ -> return ()

--------------------------------------------------------------------------------

render :: Player -> RenderContext -> RenderAction
render x ctx = withZIndex x $ locate x $ renderComposition
    [ renderDebug
    , translateY 0.8 $ renderComposition
        [ renderBody
        , renderEffects x ]
    ]
    where
    renderDebug = renderComposition $ localDebug <> globalDebug
    renderBody
        = translateY 0.8
        $ Animation.renderAnimation (x^.animationState) (x^.bodyAnimation)

    localDebug = map snd
        $ filter (\(f, _) -> x^.debugFlags.f)
        [ (drawPickupRange, renderPickupRange)
        ]

    globalDebug = map snd
        $ filter (\(f, _) -> Set.member f $ ctx^.debugFlags)
        [ (DebugFlag_ShowCollisionShapes, renderCollisionShape cs)
        ]

    cs = x^.collisionShape

    rangeScale = defaultPickupRange^._Wrapped
    renderPickupRange = T.scale rangeScale $ renderShape $ def
        & shapeType   .~ SimpleCircle
        & color       .~ Color.withOpacity Color.red 0.3

thisOracle :: Player -> EntityOracle
thisOracle x = def
   & location       .~ Just (x^.location)
   & equipment      .~ Just (x^.equipment)
   & collisionShape .~ (locate x <$> x^.collisionShape)
   & reactivity     .~ (x^.reactivity)

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

makePlayer :: Resources -> PlayerInit -> Player
makePlayer rs p = def
    & bodyAnimation .~ as
    & reactivity    .~ (p^.reactivity)
    where
    as = mconcat $ map (Animation.makeAnimation rs)
       $ mapMaybe (flip lookupAnimation rs) (p^.body)

