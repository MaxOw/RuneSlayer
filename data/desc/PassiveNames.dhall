let types      = ./Types.dhall

let PassiveTypeName = types.PassiveTypeName
let makeName = PassiveTypeName.MakePassiveTypeName

in
{ helmet           = makeName "Helmet"
, dagger           = makeName "Dagger"
, spear            = makeName "Spear"
, sword            = makeName "Sword"
, bow              = makeName "Bow"
, arrow            = makeName "Arrow"
, quiver           = makeName "Quiver"
, healthPotion     = makeName "Health Potion"
, emptyFlask       = makeName "Empty Flask"
, bag              = makeName "Bag"
, batCorpse        = makeName "Bat Corpse"
, spiderCorpse     = makeName "Spider Corpse"
, humanCorpse      = makeName "Human Corpse"
, woodenChest      = makeName "Wooden Chest"
, woodenChest_open = makeName "Wooden Chest(Open)"
, woodenDoor       = makeName "Wooden Door"
, woodenDoor_open  = makeName "Wooden Door(Open)"
, tree             = makeName "Tree"

, firepit           = makeName "Fire Pit"
, campfire          = makeName "Campfire"
, firewoodPileBig   = makeName "Firewood Pile Big"
, firewoodPile      = makeName "Firewood Pile"
, firewoodPileSmall = makeName "Firewood Pile Small"
, firewood          = makeName "Firewood"
, woodChoopingBlock = makeName "Wood Chooping Block"
, treeStump         = makeName "Tree Stump"
, barrel            = makeName "Barrel"
}
