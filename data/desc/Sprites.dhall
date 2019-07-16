let utils = ./Sprite/Utils.dhall
let paths = ./ResourcePaths.dhall

let makeSprite = utils.makeSprite
let makeItem   = utils.makeItem
let makeEnv    = utils.makeEnv
let makeEnv2   = utils.makeEnv2
let makeDoor   = utils.makeDoor

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

, barrel            = makeEnv 38 29 2 3

, firepit           = makeEnv2 31 15 1 2
, campfire          = makeEnv2 26 17 1 1
, firewoodPileBig   = makeEnv2 14 20 2 2
, firewoodPileSmall = makeEnv2 13 22 1 1
, firewoodPile      = makeEnv2 12 21 1 1
, firewood          = makeEnv2 13 21 1 1
, woodChoopingBlock = makeEnv2 14 22 1 1
, treeStump         = makeEnv2 15 23 1 1

, woodenDoor        = makeDoor 0 9
, woodenDoor_open   = makeDoor 0 15

, treeTrunk   = makeEnv 50 38 6 6
, treeFoliage = makeEnv 48 24 6 6

, bat = makeSprite paths.bat
, batCorpse = utils.selectSpritePart 32 0 0 (makeSprite paths.bat)
, spiderCorpse01 = utils.selectSpritePart 64 3 4 (makeSprite paths.spider01)
}
