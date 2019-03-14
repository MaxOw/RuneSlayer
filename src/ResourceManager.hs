module ResourceManager
    ( Resources, Resource
    , SpriteName, SpriteDesc
    , lookupResource
    , lookupSprite
    ) where

import Delude
import Engine (Img)
import Types.ResourceManager
import qualified Data.HashMap.Strict as HashMap

lookupResource :: Resource -> Resources -> Maybe Img
lookupResource r = fmap f . HashMap.lookup (r^.path) . view resourceMap
    where
    f = set part (view part r)

lookupSprite :: SpriteName -> Resources -> Maybe (SpriteDesc, Img)
lookupSprite n r = do
    d <- HashMap.lookup n (r^.spriteMap)
    i <- HashMap.lookup (d^.path) (r^.resourceMap)
    return (d, set part (view part d) i)
