let utils = ./Sprite/Utils.dhall
let paths = ./ResourcePaths.dhall

let makeSprite = utils.makeSprite
let makeItem   = utils.makeItem
let makeEnv    = utils.makeEnv

in
{ healthPotion = makeSprite "Health Potion" paths.healthPotion

, bag    = makeItem "Bag"    4 2
, helmet = makeItem "Helmet" 1 1

, treeTrunk   = makeEnv "Tree Trunk"   50 38 6 6
, treeFoliage = makeEnv "Tree Foliage" 48 24 6 6

, bat = makeSprite "Bat" paths.bat
}
