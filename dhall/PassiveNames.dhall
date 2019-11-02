let types      = ./Types.dhall

let PassiveTypeName = types.PassiveTypeName
let makeName = PassiveTypeName.MakePassiveTypeName

in
{ helmet           = makeName "helmet"
, dagger           = makeName "dagger"
, spear            = makeName "spear"
, sword            = makeName "sword"
, bow              = makeName "bow"
, arrow            = makeName "arrow"
, quiver           = makeName "quiver"
, healthPotion     = makeName "healthPotion"
, emptyFlask       = makeName "emptyFlask"
, bag              = makeName "bag"
, batCorpse        = makeName "batCorpse"
, spiderCorpse     = makeName "spiderCorpse"
, humanCorpse      = makeName "humanCorpse"
, woodenChest      = makeName "woodenChest"
, woodenChest_open = makeName "woodenChest_open"
, woodenDoor       = makeName "woodenDoor"
, woodenDoor_open  = makeName "woodenDoor_open"
, tree             = makeName "tree"

, firepit           = makeName "firePit"
, campfire          = makeName "campfire"
, firewoodPileBig   = makeName "firewoodPileBig"
, firewoodPile      = makeName "firewoodPile"
, firewoodPileSmall = makeName "firewoodPileSmall"
, firewood          = makeName "firewood"
, woodChoopingBlock = makeName "woodChoopingBlock"
, treeStump         = makeName "treeStump"
, barrel            = makeName "barrel"
}
