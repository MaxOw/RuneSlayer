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
    EntityAction_ExecuteAttack       -> handleOnUpdate   a x
    _ -> x
    where
    setAnimationKind k _ = x
        & animation.current.kind .~ k
        & animation.current.era  .~ 0
        & animation.progression  .~ Animation.defaultTransition

update :: Player -> EntityContext -> Q (Maybe Player, [DirectedAction])
update x ctx = runUpdate x ctx $ do
    whenMatch _EntityAction_ExecuteAttack executeAttack
    updateAnimation
    playerIntegrateLocation
    separateCollision
    autoTarget
    whenMatch _EntityAction_AddItem  addItems
    whenMatch _EntityAction_DropAllItems dropAllItems
    mapM_ processAction =<< use (self.processOnUpdate)
    self.processOnUpdate .= mempty

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
    -- TODO: Choose attack animation based on weapon
    -- TODO: Only execute attack if dist is withing weapon range
    -- let dist = norm vectorToTarget
    self.animation.current.direction %= Animation.vecToDir vectorToTarget
    self.animation.current.kind .= Animation.Slash
    self.animation.current.era  .= 0
    self.animation.progression  .= Animation.defaultTransition

    let attackPower = AttackPower 1 -- TODO: Calculate attack power
    addAction targetEntity $ EntityAction_SelfAttacked attackPower

playerIntegrateLocation :: Update Player ()
playerIntegrateLocation = do
    k <- use $ self.animation.current.kind
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
    -- , renderShape shape & scale 0.3
    , translateY 0.8 $ mempty {- renderAnimaiton x ctx
        [ Resource.maleBody
        , Resource.malePants
        , Resource.maleShirt
        , Resource.maleHair
        ]-}
    ]
    where
    renderDebug = renderComposition $ localDebug <> globalDebug

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
