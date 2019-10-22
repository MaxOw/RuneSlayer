{-# Language CPP #-}
module Entity
    ( module Entity
    , module Types.Entity
    ) where

import Delude
import Types.Equipment (EquipmentSlot, Equipment)
import Types.Entity.Passive (PassiveType, InteractionName)
import Types.Entity.Reactivity (ReactivCategory, ReactivValue)
import Types.Entity.Animation (AnimationName)
import Types.Entity.Agent (AgentType, PlayerStatus)
import Types.Collider (Shape, CollideWith)
import Types.Entity
import Types.Entity.Common

makeEntity :: EntityParts p -> p -> Entity
makeEntity me = f
    where
    f p = Entity
        { entityActOn  = f . makeActOn me p
        , entityUpdate = fmap (over _1 (fmap f)) . makeUpdate me p
        , entityRender = makeRender me p
        , entityOracle = makeOracle me p
        , entitySave   = makeSave me p
        , entityKind   = makeKind me p
        }

--------------------------------------------------------------------------------

oracleGetter :: EntityQuery a -> Getter Entity (Maybe a)
oracleGetter q = to (flip entityOracle q)

#define MakeOracleGetter(N,T) \
    oracle/**/N :: Getter Entity (Maybe (T)); \
    oracle/**/N = oracleGetter EntityQuery_/**/N

MakeOracleGetter(Name               , Text)
MakeOracleGetter(DisplayName        , Text)
MakeOracleGetter(Description        , Text)
MakeOracleGetter(Location           , Location)
MakeOracleGetter(Equipment          , Equipment)
MakeOracleGetter(PassiveType        , PassiveType)
MakeOracleGetter(AgentType          , AgentType)
MakeOracleGetter(Content            , [EntityId])
MakeOracleGetter(Volume             , Volume)
MakeOracleGetter(MaxVolume          , Volume)
MakeOracleGetter(StandingWeight     , Weight)
MakeOracleGetter(FittingSlots       , Set EquipmentSlot)
MakeOracleGetter(CollisionShape     , Shape)
MakeOracleGetter(CollisionBits      , BitSet32 CollideWith)
MakeOracleGetter(Reactivity         , Map ReactivCategory ReactivValue)
MakeOracleGetter(ItemAnimation      , AnimationName)
MakeOracleGetter(BehindBody         , Bool)
MakeOracleGetter(PlayerStatus       , PlayerStatus)
MakeOracleGetter(Status             , Set EntityStatus)
MakeOracleGetter(Stats              , Stats)
MakeOracleGetter(Interactions       , [InteractionName])
MakeOracleGetter(PrimaryInteraction , InteractionName)
MakeOracleGetter(Owner              , EntityId)
MakeOracleGetter(LabelOffset        , V2D)
