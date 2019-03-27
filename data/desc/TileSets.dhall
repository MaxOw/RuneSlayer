let paths   = ./ResourcePaths.dhall
let utils   = ./Sprite/Utils.dhall

let makeTileSet =
  λ(i : Natural) →
  λ(n : Text) →
  λ(p : Text) →
    { name = n
    , desc = { Standard = utils.makeSprite "" p }
    , zindex = i
    }

in
{ dirtDry = makeTileSet 100 "Dry Dirt" paths.tilesetDirtDry
, dirtWet = makeTileSet 100 "Wet Dirt" paths.tilesetDirtWet
, grass   = makeTileSet 600 "Grass"    paths.tilesetGrass
, water   = makeTileSet 400 "Water"    paths.tilesetWater
}
