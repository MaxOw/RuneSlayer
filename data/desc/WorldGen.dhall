let tilesets = ./TileSets.dhall
let statics  = ./StaticTypes.dhall

let grassLayer =
  { tileset = tilesets.grass.name
  , statics = [ statics.tree.name ]
  }

in
{ size = [200, 200]
, seed = 29
, baseTileSet = tilesets.water.name
, baseLandTileSet = tilesets.dirtWet.name
, coveringLayers = [ grassLayer ]
}
