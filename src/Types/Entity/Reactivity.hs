module Types.Entity.Reactivity where

import Delude

--------------------------------------------------------------------------------

data ReactivCategory
   = ReactivCategory_Shadow
   | ReactivCategory_Light
   | ReactivCategory_Poison
   | ReactivCategory_Blood
   | ReactivCategory_Life
   deriving (Eq, Ord, Generic)

newtype ReactivValue = ReactivValue Float
    deriving (Generic, ToJSON, FromJSON)

data Reactivity = Reactivity
   { field_category :: ReactivCategory
   , field_value    :: ReactivValue
   } deriving (Generic)

--------------------------------------------------------------------------------

instance ToJSON   ReactivCategory where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON ReactivCategory where
    parseJSON  = genericParseJSON  customOptionsJSON
instance ToJSONKey   ReactivCategory where toJSONKey   = defaultToJSONKey
instance FromJSONKey ReactivCategory where fromJSONKey = defaultFromJSONKey

instance ToJSON   Reactivity where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON Reactivity where
    parseJSON  = genericParseJSON  customOptionsJSON
