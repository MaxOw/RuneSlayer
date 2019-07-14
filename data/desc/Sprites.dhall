let utils = ./Sprite/Utils.dhall
let paths = ./ResourcePaths.dhall

let makeSprite = utils.makeSprite
let makeItem   = utils.makeItem
let makeEnv    = utils.makeEnv

in
{ healthPotion = makeSprite paths.healthPotion
, emptyFlask   = makeSprite paths.emptyFlask

, bag    = makeItem  4  2
, helmet = makeItem  1  1
, dagger = makeItem 15  8
, spear  = makeItem  2  7
, bow    = makeItem  5  1
-- , arrow  = makeSprite paths.arrow
, arrow  = utils.selectSpritePart 64 12 19 (makeSprite paths.arrowAnimation)
, quiver = utils.selectSpritePart 64 12 19 (makeSprite paths.quiverAnimation)

, woodenChest1      = makeEnv 42 34 2 2
, woodenChest1_open = makeEnv 42 36 2 2
, woodenChest2      = makeEnv 44 34 2 2
, woodenChest2_open = makeEnv 44 36 2 2

, treeTrunk   = makeEnv 50 38 6 6
, treeFoliage = makeEnv 48 24 6 6

, bat = makeSprite paths.bat
, batCorpse = utils.selectSpritePart 32 0 0 (makeSprite paths.bat)
, spiderCorpse01 = utils.selectSpritePart 64 3 4 (makeSprite paths.spider01)
}
