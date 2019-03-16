{-# Language TemplateHaskell #-}
module Types.Sprite where

import Delude
import Engine.Common.Types (Rect)

newtype SpriteName = SpriteName { unSpriteName :: Text }
    deriving (Generic, Eq, Hashable, ToJSON, FromJSON, Show)

data SpriteDesc = SpriteDesc
   { spriteDesc_name          :: SpriteName
   , spriteDesc_path          :: Text
   , spriteDesc_part          :: Maybe (Rect Int)
   , spriteDesc_pixelsPerUnit :: Maybe Int
   } deriving (Generic, Show)
makeFieldsCustom ''SpriteDesc
instance Default SpriteDesc where
    def = SpriteDesc
        { spriteDesc_name          = SpriteName ""
        , spriteDesc_path          = def
        , spriteDesc_part          = Nothing
        , spriteDesc_pixelsPerUnit = Nothing
        }

instance ToJSON SpriteDesc where
    toEncoding = genericToEncoding customOptionsJSON
instance FromJSON SpriteDesc where
    parseJSON = genericParseJSON customOptionsJSON

