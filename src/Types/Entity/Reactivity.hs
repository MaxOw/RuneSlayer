module Types.Entity.Reactivity where

import Delude
import Data.Aeson (ToJSONKey(..), FromJSONKey(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson

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
instance ToJSONKey   ReactivCategory where
    toJSONKey = Aeson.ToJSONKeyText f (Aeson.text . f)
        where f = decodeUtf8 . Aeson.encode
instance FromJSONKey ReactivCategory where
    fromJSONKey = Aeson.FromJSONKeyTextParser (parseJSON . Aeson.String)

instance ToJSON   Reactivity where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON Reactivity where
    parseJSON  = genericParseJSON  customOptionsJSON
