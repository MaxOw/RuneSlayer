module Types.Sprite where

import Delude
import Engine.Common.Types (Rect)

newtype SpriteName = SpriteName { unSpriteName :: Text }
    deriving (Generic, Eq, Hashable, ToJSON, FromJSON, Show)

data SpriteDesc = SpriteDesc
   { field_name          :: SpriteName
   , field_path          :: Text
   , field_part          :: Maybe (Rect Int)
   , field_pixelsPerUnit :: Maybe Int
   } deriving (Generic, Show)

instance Default SpriteDesc where
    def = SpriteDesc
        { field_name          = SpriteName ""
        , field_path          = def
        , field_part          = Nothing
        , field_pixelsPerUnit = Nothing
        }

instance ToJSON SpriteDesc where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON SpriteDesc where
    parseJSON = genericParseJSON customOptionsJSON

