module Schema where

import Delude
import Codec.Winery

import Types.Entity (Entity)
import Types.Schema.WorldMap
import Entity.Tile (makeTile)
import EntityLike (toEntity)
import ResourceManager (Resources, lookupTileSet)
import EntityIndex (EntityIndex)
import qualified EntityIndex

loadWorldMap :: MonadIO m => FilePath -> m WorldMap
loadWorldMap = liftIO . readFileDeserialise

saveWorldMap :: MonadIO m => FilePath -> WorldMap -> m ()
saveWorldMap fp wm = liftIO $ writeFileSerialise fp wm

loadWorldMapToIndex :: MonadIO m => FilePath -> Resources -> EntityIndex -> m ()
loadWorldMapToIndex fp rs eix = do
    wm <- loadWorldMap fp
    let ts = mapMaybe (tileToEntity rs) $ toList $ wm^.ff#tiles
    forM_ ts $ EntityIndex.insert ?? eix

tileToEntity :: Resources -> PlaceTileAt -> Maybe Entity
tileToEntity rs p = toEntity . set location loc . makeTile tr <$> ts
    where
    loc = p^.ff#location
    tr = p^.ff#tileRole
    ts = lookupTileSet (p^.ff#tileSet) rs

