module ResourceManager
    ( Resources
    , SpriteDesc
    , lookupImg
    , lookupSprite
    , lookupAnimation
    , lookupTileSet
    , lookupStaticEntity

    , renderSprite
    ) where

import Delude
import Engine (Img, RenderAction, renderImg)
import qualified Diagrams.TwoD.Transform as T
import Types.ResourceManager
import Types.Entity.Animation (AnimationDesc)
import Types.Entity.TileSet   (TileSetName, TileSet)
import Types.Entity.StaticEntity
import qualified Data.HashMap.Strict as HashMap

lookupImg :: FilePath -> Resources -> Maybe Img
lookupImg p = HashMap.lookup (fromString p) . view resourceMap

lookupSprite :: SpriteDesc -> Resources -> Maybe Img
lookupSprite r = fmap f . HashMap.lookup (r^.path) . view resourceMap
    where
    f = set part (view part r)

lookupAnimation :: Text -> Resources -> Maybe AnimationDesc
lookupAnimation x = HashMap.lookup x . view animationsMap

lookupTileSet :: TileSetName -> Resources -> Maybe TileSet
lookupTileSet x = HashMap.lookup x . view tileSetMap

lookupStaticEntity :: StaticEntityTypeName -> Resources -> Maybe StaticEntityType
lookupStaticEntity x = HashMap.lookup x . view staticMap

renderSprite :: Resources -> SpriteDesc -> RenderAction
renderSprite rs s = case lookupSprite s rs of
    Nothing  -> mempty -- renderShape shape
    Just img -> renderImg (set part (s^.part) img)
        & sscale (s^.pixelsPerUnit)
    where
    sscale = maybe id (T.scale . (1/) . fromIntegral)
    {-
    shape = def
        & shapeType   .~ SimpleSquare
        & color       .~ Color.opaque Color.gray
    -}

