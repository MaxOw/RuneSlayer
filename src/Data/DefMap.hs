module Data.DefMap
    ( DefMap
    , setDefaultValue
    , insert, delete, lookup
    , mapMaybe
    ) where

import Delude hiding (mapMaybe)
import qualified Data.Map as Map

--------------------------------------------------------------------------------

data DefMap k v = DefMap
   { field_defaultValue :: v
   , field_baseMap      :: Map k v
   } deriving (Generic)

instance Default v => Default (DefMap k v)

defaultValue :: Lens' (DefMap k v) v
defaultValue = ff#defaultValue

baseMap :: Lens' (DefMap k v) (Map k v)
baseMap = ff#baseMap

--------------------------------------------------------------------------------

setDefaultValue :: v -> DefMap k v -> DefMap k v
setDefaultValue = set defaultValue

insert :: Ord k => k -> v -> DefMap k v -> DefMap k v
insert k v = over baseMap $ Map.insert k v

delete :: Ord k => k -> DefMap k v -> DefMap k v
delete k = over baseMap $ Map.delete k

lookup :: Ord k => k -> DefMap k v -> v
lookup k m = fromMaybe (m^.defaultValue) $ Map.lookup k (m^.baseMap)

mapMaybe :: (v -> Maybe v) -> DefMap k v -> DefMap k v
mapMaybe f = over baseMap (Map.mapMaybe f)
