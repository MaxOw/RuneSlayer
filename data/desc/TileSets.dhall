let paths   = ./ResourcePaths.dhall
let types   = ./Types.dhall
let utils   = ./Sprite/Utils.dhall

let Path = types.Path

let makeTileSet =
  λ(i : Natural) →
  λ(n : Text) →
  λ(p : Path) →
    { name = n
    , color = None Text
    , desc = { Standard = utils.makeSprite p }
    , zindex = i
    }

in
{ dirtDry = makeTileSet 100 "Dry Dirt" paths.tilesetDirtDry
, dirtWet = makeTileSet 100 "Wet Dirt" paths.tilesetDirtWet
, water   = makeTileSet 400 "Water"    paths.tilesetWater

, grass   = makeTileSet 600 "Grass"    paths.tilesetGrass //
    { color = Some "green" }
}
