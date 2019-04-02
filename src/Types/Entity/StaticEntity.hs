module Types.Entity.StaticEntity where

import Delude

import Types.Entity.Common
import Types.Entity.ZIndex
import Types.Entity.Appearance

newtype StaticEntityTypeName
    = StaticEntityTypeName { unStaticEntityTypeName :: Text }
    deriving (Default, Eq, Hashable, Generic, ToJSON, FromJSON)
data StaticEntityType = StaticEntityType
   { field_name           :: StaticEntityTypeName
   , field_appearance     :: Appearance
   -- , field_collisionShape :: Maybe CollisionShape
   } deriving (Generic)

data StaticEntity = StaticEntity
   { field_location   :: Location
   , field_entityType :: StaticEntityType
   } deriving (Generic)

--------------------------------------------------------------------------------

instance Default StaticEntityType
instance Default StaticEntity

instance ToJSON StaticEntityType where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON StaticEntityType where
    parseJSON = genericParseJSON customOptionsJSON

instance ToJSON StaticEntity where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON StaticEntity where
    parseJSON = genericParseJSON customOptionsJSON

instance GetZIndex StaticEntity Word32 where
    get_zindex _ = toZIndex EntityZIndex_Vertical



