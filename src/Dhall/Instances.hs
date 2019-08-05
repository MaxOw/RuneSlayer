{-# Language ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Dhall.Instances () where

import Relude
import qualified Data.Functor.Foldable as F
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Dhall.Fix (autoWithFix)
import Dhall

import Linear
import Engine.Common.Types (Size(MkSize))

import Types.EntityAction
import Types.Entity.Common
import Types.Entity.Animation
import Types.Entity.PassiveType
import Types.Entity.Agent (AgentTypeName(..))
import Types.Entity.TileSet
import Types.Equipment
import Types.WorldGen

--------------------------------------------------------------------------------

autoWithNewtype :: forall a t. Interpret a => Text -> (a -> t) -> Dhall.Type t
autoWithNewtype n f = fmap f $ union $ constructor ("Make" <> n) (auto @a)

instance Interpret Int where autoWith = fmap fromIntegral . autoWith @Integer
instance Interpret Float where autoWith = fmap realToFrac . autoWith @Double

instance Interpret t => Interpret (V2 t) where
    autoWith opts = record $ V2
        <$> field "x" (autoWith opts)
        <*> field "y" (autoWith opts)

instance Interpret Location where autoWith = fmap Location . autoWith @(V2 Float)
instance Interpret Probability where
    autoWith _ = autoWithNewtype "Probability" Probability
instance Interpret PassiveTypeName where
    autoWith _ = autoWithNewtype "PassiveTypeName" PassiveTypeName
instance Interpret AgentTypeName where
    autoWith _ = autoWithNewtype "AgentTypeName" AgentTypeName
instance Interpret Direction

instance Interpret a => Interpret (Size a) where
    autoWith opts = fmap MkSize $
        record $ V2
            <$> field "width"  (autoWith opts)
            <*> field "height" (autoWith opts)

instance Interpret TileSetName where  autoWith = fmap TileSetName . autoWith @Text
instance Interpret CoveringLayer
instance Interpret WorldGenConfig

data EntityValueDhall
   = EntityValueDhall_Location  Location
   | EntityValueDhall_Direction Direction
   deriving (Generic, Show)

data EntityActionDhall
   = EntityActionDhall_SetValue EntityValueDhall
   | EntityActionDhall_AddLoadout
        [LoadoutEntry (Spawn PassiveTypeName EntityActionDhall)]
   deriving (Generic, Show)

instance Interpret EquipmentSlot
instance Interpret a => Interpret (LoadoutEntry a)
instance Interpret EntityValueDhall
instance (Interpret n, Interpret a) => Interpret (Spawn n a)
instance Interpret e => Interpret (SelectionEntry e)
instance Interpret e => Interpret (Range e)

makeBaseFunctor ''EntityActionDhall
deriving instance Generic (EntityActionDhallF t)
instance Interpret t => Interpret (EntityActionDhallF t)
instance Interpret EntityActionDhall where autoWith = autoWithFix

makeBaseFunctor ''EntityAction

instance Interpret EntityAction where
    autoWith opts = fromEntityActionDhall <$> autoWith @EntityActionDhall opts

fromEntityActionDhall :: EntityActionDhall -> EntityAction
fromEntityActionDhall = F.hoist $ \case
    EntityActionDhall_SetValueF   v -> EntityAction_SetValueF $ fromDhallValue v
    EntityActionDhall_AddLoadoutF l -> EntityAction_AddLoadoutF l
    where
    fromDhallValue = \case
        EntityValueDhall_Location  x -> EntityValue_Location  x
        EntityValueDhall_Direction x -> EntityValue_Direction x

