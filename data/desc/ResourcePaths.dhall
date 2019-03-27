let makeImgsPath = λ(x : Text) → "data/imgs/${x}.png"
let makeCharPath = λ(x : Text) → makeImgsPath "characters/${x}"
let makeUnitPath = λ(x : Text) → makeImgsPath "units/${x}"
let makeTileSetPath = λ(x : Text) → makeImgsPath "tilesets/twosided/${x}"

in
{ itemsAtlas1 = makeImgsPath "items1/items1"
, envAtlas1   = makeImgsPath "environment/base_out_atlas"

, tilesetDirtDry = makeTileSetPath "dirt"
, tilesetDirtWet = makeTileSetPath "dirt2"
, tilesetGrass   = makeTileSetPath "grass"
, tilesetWater   = makeTileSetPath "water"

, maleBodyLight      = makeCharPath "body/male/light"
, maleHairPlainBrown = makeCharPath "hair/male/plain/brown"
, malePantsTeal      = makeCharPath "legs/pants/male/teal_pants_male"
, maleShirtWhite     = makeCharPath "torso/shirts/longsleeve/male/white_longsleeve"

, bat = makeUnitPath "bat"

, healthPotion = makeImgsPath "items0/P_Red07"
}
