module Entity.Unit
    ( Unit, unitToEntity

    , makeUnit
    ) where

import Delude

import Entity
import Types.Entity.Unit
import Types.Entity.Timer
import Types.Entity.Effect
import Types.ResourceManager (Resources)

import Entity.Utils
import Entity.Actions
import qualified Entity.Animation as Animation

--------------------------------------------------------------------------------

actOn :: Unit -> EntityAction -> Unit
actOn x a = x & case a of
    EntityAction_SelfMarkAsTarget   -> set isMarked True
    EntityAction_SelfUnmarkAsTarget -> set isMarked False
    EntityAction_SelfAttacked     _ -> handleOnUpdate a
    _ -> id

update :: Unit -> EntityContext -> Q (Maybe Unit, [DirectedAction])
update x ctx = runUpdate x ctx $ do
    decideAction
    updateAnimationState
    updateTimer
    integrateLocation
    anyMatch _EntityAction_SelfAttacked procAttacked
    self.processOnUpdate .= mempty

decideAction :: Update Unit ()
decideAction = do
    whenNothingM_ (use $ self.target) $ do
        aggroRange <- use $ self.unitType.ff#aggroRange
        es <- queryInRadius EntityKind_Dynamic aggroRange
        ht <- use $ self.unitType.ff#hostileTowards
        let tid = view entityId <$> listToMaybe (filter (isHostileTo ht) es)
        self.target .= tid
    self.velocity .= 0
    whenJustM getTarget $ \t -> do
        attackRange <- use $ self.unitType.ff#attackRange
        pursueRange <- use $ self.unitType.ff#pursueRange
        dist <- distanceToEntity pursueRange t
        if dist < attackRange
        then attackTarget t
        else if dist < pursueRange
            then moveTowards t
            else self.target .= Nothing
    where
    getTarget = do
        mtid <- use $ self.target
        case mtid of
            Nothing -> return Nothing
            Just ti -> queryById ti

attackTarget :: EntityWithId -> Update Unit ()
attackTarget targetEntity = do
    whenM (checkTimeUp Timer_Attack) $ do
        attackPower <- use $ self.unitType.ff#attackPower
        attackSpeed <- use $ self.unitType.ff#attackSpeed
        addAction targetEntity $ EntityAction_SelfAttacked attackPower
        startTimer Timer_Attack attackSpeed

procAttacked :: NonEmpty AttackPower -> Update Unit ()
procAttacked as = do
    ds <- mapM applyDefence as
    mapM_ (addEffect . HitEffect) ds
    let p = sumOf (traverse._Wrapped) ds
    self.health._Wrapped -= p
    whenM shouldDie $ do
        deleteSelf .= True
        loc <- use $ self.location
        mco <- use $ self.unitType.corpse
        dir <- use $ self.animationState.current.direction
        whenJust mco $ \c ->
            addWorldAction $ WorldAction_SpawnEntity $ SpawnEntity_Item $ def
                & location  .~ loc
                & name      .~ c
                & direction .~ Just dir
    where
    applyDefence     x = return x

render :: Unit -> RenderContext -> RenderAction
render x _ctx = withZIndex x $ locate x $ renderComposition
    [ renderIf (x^.isMarked) renderTargetMark
    , translateY 0.8 $ renderComposition
        [ Animation.renderAnimation (x^.animationState) (x^.animation)
        ]
    ]

oracle :: Unit -> EntityQuery a -> Maybe a
oracle x = \case
    EntityQuery_Location   -> Just $ x^.location
    EntityQuery_Reactivity -> Just $ x^.unitType.reactivity
    _                      -> Nothing

--------------------------------------------------------------------------------

unitToEntity :: Unit -> Entity
unitToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = oracle
   , makeSave   = Just . EntitySum_Unit
   , makeKind   = EntityKind_Dynamic
   }

--------------------------------------------------------------------------------

makeUnit :: Resources -> UnitType -> Unit
makeUnit rs t = def
    & unitType .~ t
    & health   .~ (t^.maxHealth)
    & animation .~ Animation.makeAnimation rs (t^.animation)
    & animationState.progression .~ Animation.Cycle

