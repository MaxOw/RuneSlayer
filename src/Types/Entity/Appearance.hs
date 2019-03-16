module Types.Entity.Appearance where

import Delude
import Types.Sprite (SpriteName)

data Appearance
   = Appearance_Sprite SpriteName
   | Appearance_Translate { _vector :: (V2 Float), _content :: Appearance }
   | Appearance_Compose [Appearance]
   deriving (Generic)
instance Default Appearance where
    def = Appearance_Compose []

instance ToJSON Appearance where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON Appearance where
    parseJSON = genericParseJSON customOptionsJSON

