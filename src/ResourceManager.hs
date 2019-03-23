module ResourceManager
    ( Resources, Resource
    , SpriteName, SpriteDesc
    , lookupResource
    , lookupImg
    , lookupSprite
    , lookupSpriteName
    , lookupAnimation

    , renderSprite
    ) where

import Delude
import Engine (Img, RenderAction, renderImg)
import Diagrams.TwoD.Transform (scale)
import Types.ResourceManager
import Types.Entity.Animation (AnimationDesc)
import qualified Data.HashMap.Strict as HashMap

lookupResource :: Resource -> Resources -> Maybe Img
lookupResource r = fmap f . HashMap.lookup (r^.path) . view resourceMap
    where
    f = set part (view part r)

lookupImg :: FilePath -> Resources -> Maybe Img
lookupImg p = HashMap.lookup (fromString p) . view resourceMap

lookupSprite :: SpriteDesc -> Resources -> Maybe Img
lookupSprite r = fmap f . HashMap.lookup (r^.path) . view resourceMap
    where
    f = set part (view part r)

lookupSpriteName :: SpriteName -> Resources -> Maybe (SpriteDesc, Img)
lookupSpriteName n r = do
    d <- HashMap.lookup n (r^.spriteMap)
    i <- HashMap.lookup (d^.path) (r^.resourceMap)
    return (d, set part (view part d) i)

lookupAnimation :: Text -> Resources -> Maybe AnimationDesc
lookupAnimation x = HashMap.lookup x . view animationsMap

renderSprite :: Resources -> SpriteDesc -> RenderAction
renderSprite rs s = case lookupSprite s rs of
    Nothing  -> mempty -- renderShape shape
    Just img -> renderImg (set part (s^.part) img)
        & sscale (s^.pixelsPerUnit)
    where
    sscale = maybe id (scale . (1/) . fromIntegral)
    {-
    shape = def
        & shapeType   .~ SimpleSquare
        & color       .~ Color.opaque Color.gray
    -}

