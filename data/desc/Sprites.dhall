let utils = ./Sprite/Utils.dhall
let paths = ./ResourcePaths.dhall

let makeSprite = utils.makeSprite
let makeItem   = utils.makeItem
let makeEnv    = utils.makeEnv

in
{ healthPotion = makeSprite paths.healthPotion

, bag    = makeItem  4  2
, helmet = makeItem  1  1
, dagger = makeItem 15  8
, spear  = makeItem  2  7
, bow    = makeItem  5  1
-- , arrow  = makeSprite paths.arrow
, arrow  = utils.selectSpritePart 64 12 19 (makeSprite paths.arrowAnimation)

, treeTrunk   = makeEnv 50 38 6 6
, treeFoliage = makeEnv 48 24 6 6

, bat = makeSprite paths.bat
, batCorpse = utils.selectSpritePart 32 0 0 (makeSprite paths.bat)
, spiderCorpse01 = utils.selectSpritePart 64 3 4 (makeSprite paths.spider01)
}
