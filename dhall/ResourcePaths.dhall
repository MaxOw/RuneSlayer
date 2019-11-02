let types = ./Types.dhall
let makePath = types.Path.MakePath

let makeImgsPath = λ(x : Text) → makePath "data/imgs/${x}.png"
let makeCharPath = λ(x : Text) → makeImgsPath "characters/${x}"
let makeUnitPath = λ(x : Text) → makeImgsPath "units/${x}"
let makeTileSetPath = λ(x : Text) → makeImgsPath "tilesets/twosided/${x}"

in
{ itemsAtlas1 = makeImgsPath "items1/items1"
, envAtlas1   = makeImgsPath "environment/base_out_atlas"
, envAtlas2   = makeImgsPath "environment/decoration_medieval/decorations-medieval"
, doors       = makeImgsPath "environment/doors-animated"

, tilesetDirtDry = makeTileSetPath "dirt"
, tilesetDirtWet = makeTileSetPath "dirt2"
, tilesetGrass   = makeTileSetPath "grass"
, tilesetWater   = makeTileSetPath "water"

, maleBodyLight  = makeCharPath "body/male/light"
, malePantsTeal  = makeCharPath "legs/pants/male/teal_pants_male"
, maleShirtWhite = makeCharPath "torso/shirts/longsleeve/male/white_longsleeve"

, maleHairBangsLongBrown = makeCharPath "hair/male/bangslong/brown"
, maleHairPlainBrown     = makeCharPath "hair/male/plain/brown"
, maleBeardBrown         = makeCharPath "facial/male/beard/brown"

, helmetAnimation = makeCharPath "head/helms/male/metal_helm_male"

, daggerAnimation = makeCharPath "weapons/right_hand/male/dagger_male"
, spearAnimation  = makeCharPath "weapons/right_hand/male/spear_male"
, bowAnimation    = makeCharPath "weapons/right_hand/either/bow"
, arrowAnimation  = makeCharPath "weapons/left_hand/either/arrow"
, quiverAnimation = makeCharPath "behind_body/equipment/quiver"

, oversizeSpearAnimation = makeCharPath "weapons/oversize/two_hand/either/spear"
, oversizeSwordAnimation = makeCharPath "weapons/oversize/right_hand/male/longsword_male"

, bat = makeUnitPath "bat"
, spider01 = makeUnitPath "spiders/spider01"

, healthPotion = makeImgsPath "items0/P_Red02"
, emptyFlask   = makeImgsPath "items0/I_Bottle02"
, arrow = makeImgsPath "items0/W_Arrow01"
}
