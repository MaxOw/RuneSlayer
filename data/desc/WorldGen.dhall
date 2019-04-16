let tilesets = ./TileSets.dhall

in
{ size = [200, 200]
, seed = 29
, baseTileSet = tilesets.water.name
, baseLandTileSet = tilesets.dirtWet.name
}
