module Types.Entity.Appearance where

import Delude
import Types.Sprite (SpriteName)

data Located x = Located
   { field_vector :: V2 Float
   , field_value  :: x
   } deriving Generic
newtype Appearance = Appearance [Located SpriteName]
    deriving (Generic, Default)

instance ToJSON x => ToJSON (Located x) where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON x => FromJSON (Located x) where
    parseJSON = genericParseJSON customOptionsJSON

instance ToJSON Appearance where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON Appearance where
    parseJSON = genericParseJSON customOptionsJSON

