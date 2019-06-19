{-# Language CPP #-}
module Entity
    ( module Entity
    , module Types.Entity
    ) where

import Delude
import Types.Equipment (EquipmentSlot, Equipment)
import Types.Entity.Item (ItemType)
import Types.Entity.ZIndex (EntityZIndex)
import Types.Entity.Reactivity (ReactivCategory, ReactivValue)
import Types.Entity.Animation (AnimationName)
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
        , entityKind   = makeKind me
        }

--------------------------------------------------------------------------------

oracleGetter :: EntityQuery a -> Getter Entity (Maybe a)
oracleGetter q = to (flip entityOracle q)

#define MakeOracleGetter(N,T) \
    oracle/**/N :: Getter Entity (Maybe (T)); \
    oracle/**/N = oracleGetter EntityQuery_/**/N

MakeOracleGetter(Name           , Text)
MakeOracleGetter(Location       , Location)
MakeOracleGetter(Equipment      , Equipment)
MakeOracleGetter(ItemType       , ItemType)
MakeOracleGetter(Content        , [EntityId])
MakeOracleGetter(Volume         , Volume)
MakeOracleGetter(MaxVolume      , Volume)
MakeOracleGetter(FittingSlots   , Set EquipmentSlot)
MakeOracleGetter(Zindex         , EntityZIndex)
MakeOracleGetter(CollisionShape , CollisionShape)
MakeOracleGetter(Reactivity     , Map ReactivCategory ReactivValue)
MakeOracleGetter(ItemAnimation  , AnimationName)
MakeOracleGetter(Status         , Set EntityStatus)
MakeOracleGetter(Stats          , Stats)
