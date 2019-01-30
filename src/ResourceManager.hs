module ResourceManager
    ( ResourceMap, Resource
    , lookupResource
    ) where

import Delude
import Engine (Img)
import Types.ResourceManager
import qualified Data.HashMap.Strict as HashMap

lookupResource :: Resource -> ResourceMap -> Maybe Img
lookupResource r = fmap f . HashMap.lookup (r^.path)
    where
    f = set part (view part r)
