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

, helmetAnimation = makeCharPath "head/helms/male/metal_helm_male"
, daggerAnimation = makeCharPath "weapons/right hand/male/dagger_male"
, spearAnimation  = makeCharPath "weapons/right hand/male/spear_male"
, bowAnimation    = makeCharPath "weapons/right hand/either/bow"
, arrowAnimation  = makeCharPath "weapons/left hand/either/arrow"

, bat = makeUnitPath "bat"
, spider01 = makeUnitPath "spiders/spider01"

, healthPotion = makeImgsPath "items0/P_Red02"
, emptyFlask   = makeImgsPath "items0/I_Bottle02"
, arrow = makeImgsPath "items0/W_Arrow01"
}
