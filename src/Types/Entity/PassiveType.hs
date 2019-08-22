{-# Language TemplateHaskell #-}
module Types.Entity.PassiveType where

import Delude

import Types.Entity.Common
import Types.Entity.Animation
import Types.Entity.Appearance
import Types.Equipment

--------------------------------------------------------------------------------

data WeaponKind
   = WeaponKind_Slashing
   | WeaponKind_Thrusting
   | WeaponKind_Projecting
   deriving (Eq, Generic)

data PassiveKind
   = PassiveKind_Container
   | PassiveKind_SmallItem
   | PassiveKind_BigItem
   | PassiveKind_Projectile
   | PassiveKind_Arrow
   | PassiveKind_Item
   deriving (Eq, Ord, Generic)

newtype PassiveTypeName = PassiveTypeName { unPassiveTypeName :: Text }
    deriving (Show, Eq, Hashable, Generic, ToJSON, FromJSON)

newtype InteractionName = InteractionName { unInteractionName :: Text }
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data InteractionEffect
   = InteractionEffect_TransformInto PassiveTypeName
   | InteractionEffect_InspectContent
   | InteractionEffect_DeleteSelf
   | InteractionEffect_Heal Health
   deriving (Generic, Show)

{-
data PassiveWithLoadout
   { field_name    :: PassiveTypeName
   , field_loadout :: Maybe [LoadoutEntry]
   } deriving (Generic)
-}

data SelectionEntry a = SelectionEntry
   -- (Optional) Probability/Weight that a given entry will be selected.
   -- Nothing = 1.0
   { field_probability :: Maybe Probability
   , field_entry       :: a
   } deriving (Generic, Show, Functor, Foldable, Traversable)

data Range a = Range
   { field_rangeMin :: a
   , field_rangeMax :: a
   } deriving (Generic, Show, Functor)

data LoadoutEntry a = LoadoutEntry
   -- (Optional) Probability that a given element will appear in a loadout.
   -- Nothing = Always
   { field_probability :: Maybe Probability
   -- (Optional) Equipment slot to put element in.
   -- Nothing = Just put it wherever.
   , field_slot        :: Maybe EquipmentSlot
   -- (Optional) Count of elements to be generated.
   -- (randomly selected from a range).
   -- Nothing = 1
   , field_countRange  :: Maybe (Range Natural) -- , Natural)
   -- Element for this entry will be chosen as one item from this list.
   -- Probabilities will be normalized to sum to one.
   , field_selection   :: [SelectionEntry a]
   } deriving (Generic, Show, Functor, Foldable, Traversable)

data PassiveType = PassiveType
   { field_name          :: PassiveTypeName
   , field_volume        :: Volume
   , field_passiveKind   :: Set PassiveKind
   , field_weaponKind    :: Maybe WeaponKind
   , field_stats         :: Stats
   , field_appearance    :: Appearance
   -- Animation when equipped, TODO: rename it as such.
   , field_animation     :: Maybe AnimationName
   , field_behindBody    :: Maybe Bool
   , field_fittingSlots  :: Set EquipmentSlot
   , field_containerType :: Maybe ContainerType
   , field_interactions  :: Map InteractionName [InteractionEffect]
   , field_zindex        :: Word32
   , field_renderOffset  :: Maybe V2D
   } deriving (Generic)

data ContainerType = ContainerType
   { field_maxVolume  :: Volume
   , field_allowKinds :: Set PassiveKind
   , field_showCount  :: Bool
   } deriving (Generic)

--------------------------------------------------------------------------------

instance ToJSON a => ToJSON (LoadoutEntry a) where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON a => FromJSON (LoadoutEntry a) where
    parseJSON = genericParseJSON customOptionsJSON

instance ToJSON a => ToJSON (SelectionEntry a) where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON a => FromJSON (SelectionEntry a) where
    parseJSON = genericParseJSON customOptionsJSON

instance ToJSON a => ToJSON (Range a) where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON a => FromJSON (Range a) where
    parseJSON = genericParseJSON customOptionsJSON

instance ToJSON PassiveKind where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON PassiveKind where parseJSON = genericParseJSON customOptionsJSON

instance ToJSON WeaponKind where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON WeaponKind where parseJSON = genericParseJSON customOptionsJSON

makeWrapped ''PassiveTypeName
instance Default PassiveTypeName

instance ToJSON InteractionEffect where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON InteractionEffect where
    parseJSON = genericParseJSON customOptionsJSON

instance Default PassiveType
instance ToJSON PassiveType where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON PassiveType where parseJSON = genericParseJSON customOptionsJSON

instance Default ContainerType
instance ToJSON ContainerType where toEncoding = genericToEncoding customOptionsJSON
instance FromJSON ContainerType where parseJSON = genericParseJSON customOptionsJSON
