module ResourceManager
    ( Resources, Resource
    , SpriteName, SpriteDesc
    , lookupResource
    , lookupSprite
    , lookupSpriteName
    ) where

import Delude
import Engine (Img)
import Types.ResourceManager
import qualified Data.HashMap.Strict as HashMap

lookupResource :: Resource -> Resources -> Maybe Img
lookupResource r = fmap f . HashMap.lookup (r^.path) . view resourceMap
    where
    f = set part (view part r)

lookupSprite :: SpriteDesc -> Resources -> Maybe Img
lookupSprite r = fmap f . HashMap.lookup (r^.path) . view resourceMap
    where
    f = set part (view part r)

lookupSpriteName :: SpriteName -> Resources -> Maybe (SpriteDesc, Img)
lookupSpriteName n r = do
    d <- HashMap.lookup n (r^.spriteMap)
    i <- HashMap.lookup (d^.path) (r^.resourceMap)
    return (d, set part (view part d) i)
