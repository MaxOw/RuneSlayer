{-# Language TemplateHaskell #-}
module Types.EntityOracle where

import Delude
import Types.Equipment
import Types.Entity.Common
import Types.Entity.ItemType
import Types.Entity.ZIndex

data EntityOracle = EntityOracle
   { entityOracle_name         :: Maybe Text
   , entityOracle_location     :: Maybe Location
   , entityOracle_equipment    :: Maybe Equipment
   , entityOracle_itemKind     :: Maybe ItemKind
   , entityOracle_content      :: Maybe [EntityId]
   , entityOracle_volume       :: Maybe Volume
   , entityOracle_maxVolume    :: Maybe Volume
   , entityOracle_fittingSlots :: Set EquipmentSlot
   , entityOracle_static       :: Bool
   , entityOracle_zindex       :: Maybe EntityZIndex
   } deriving (Generic)
makeFieldsCustom ''EntityOracle

instance Default EntityOracle
