
let makeImgsPath = \(x : Text) -> "data/imgs/${x}.png"
let makeCharPath = \(x : Text) -> makeImgsPath "characters/${x}"
let makeUnitPath = \(x : Text) -> makeImgsPath "units/${x}"
in
{ itemsAtlas1 = makeImgsPath "items1/items1"
, envAtlas1   = makeImgsPath "environment/base_out_atlas"

, maleBodyLight      = makeCharPath "body/male/light"
, maleHairPlainBrown = makeCharPath "hair/male/plain/brown"
, malePantsTeal      = makeCharPath "legs/pants/male/teal_pants_male"
, maleShirtWhite     = makeCharPath "torso/shirts/longsleeve/male/white_longsleeve"

, bat = makeUnitPath "bat"

, healthPotion = makeImgsPath "items0/P_Red07"
}
