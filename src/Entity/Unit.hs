module Entity.Unit
    ( Unit, unitToEntity

    , makeUnit
    , testUnitType_bat
    ) where

import Delude

import Types.Entity
import Types.Entity.Unit

import Entity.Utils
import Entity.Actions
import Types.Entity.Animation (aniMap)
import qualified Entity.Animation as Animation
import qualified Resource

--------------------------------------------------------------------------------

actOn :: Unit -> EntityAction -> Unit
actOn x a = x & case a of
    EntityAction_SelfMarkAsTarget   -> set isMarked True
    EntityAction_SelfUnmarkAsTarget -> set isMarked False
    EntityAction_SelfAttacked     _ -> handleOnUpdate a
    _ -> id

update :: Unit -> EntityContext -> Q (Maybe Unit, [DirectedAction])
update x ctx = runUpdate x ctx $ do
    -- updateAnimation
    updateEffects defaultDelta
    anyMatch _EntityAction_SelfAttacked procAttack
    self.animation %= Animation.update defaultDelta
    self.processOnUpdate .= mempty

procAttack :: NonEmpty AttackPower -> Update Unit ()
procAttack as = do
    ds <- mapM applyDefence as
    mapM_ (addEffect . Animation.HitEffect) ds
    let p = sumOf (traverse._Wrapped) ds
    self.health._Wrapped -= p
    whenM shouldDie $ do
        deleteSelf .= True
        loc <- use $ self.location
        addWorldAction $ WorldAction_SpawnEntity $ SpawnEntity_Item $ def
            & location .~ loc
            & itemType._Wrapped .~ "Health Potion"
            -- testItemType_healthPotion
        -- TODO: spawn item: self.corpse
    where
    applyDefence     x = return x

render :: Unit -> RenderContext -> RenderAction
render x ctx = withZIndex x $ locate x $ renderComposition
    [ renderIf (x^.isMarked) renderTargetMark
    , translateY 0.8 $ renderComposition
        [ renderAnimaiton x ctx [ x^.unitType.sprite ]
        , renderEffects x ]
    ]

thisOracle :: Unit -> EntityOracle
thisOracle x = def
   & location       .~ Just (x^.location)

--------------------------------------------------------------------------------

unitToEntity :: Unit -> Entity
unitToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = thisOracle
   , makeSave   = EntitySum_Unit
   , makeKind   = EntityKind_Dynamic
   }

--------------------------------------------------------------------------------

makeUnit :: UnitType -> Unit
makeUnit t = def
    & unitType  .~ t
    & animation .~ (t^.animation)
    & health    .~ (t^.maxHealth)

testUnitType_bat :: UnitType
testUnitType_bat = def
    & sprite    .~ Resource.bat
    & animation .~ anim
    & maxHealth .~ Health 10
    where
    anim = def
        & progression .~ Animation.Cycle
        & aniMap      .~ batMap

    batMap s = \r -> Resource.mkAtlasPart r fr dk
        where
        fr = max 1 $ min 3 $ floor $ 3 * (s^.era) + 1
        dk = fromEnum (s^.direction)
