let types    = ./Types.dhall
let enums    = ./Enums.dhall
let tilesets = ./TileSets.dhall
let item     = ./PassiveNames.dhall
let agents   = ./AgentNames.dhall
let actions  = ./EntityActions.dhall

let Location        = types.Location
let PassiveTypeName = types.PassiveTypeName
let AgentTypeName   = types.AgentTypeName
let EntityAction    = types.EntityAction

let EquipmentSlot   = enums.EquipmentSlot

let loadout = actions.loadout

let grassLayer =
  { tileset = tilesets.grass.name
  , statics = [ item.tree ]
  }

let makeItem =
  λ(n : PassiveTypeName) →
  λ(a : List EntityAction) →
    { name = n, actions = a }

let placeAt =
  λ(x : Double) →
  λ(y : Double) →
  λ(n : PassiveTypeName) →
    makeItem n [ actions.setLocation x y ]

let items =
  [ placeAt -0.3  5.3 item.woodenChest
  , placeAt -0.2  3.2 item.campfire
  , placeAt -2.0  2.0 item.firewood
  , placeAt -2.3  3.0 item.firewoodPileSmall
  , placeAt -2.1  4.8 item.barrel
  , placeAt -3.4  3.6 item.woodChoopingBlock
  , placeAt  2.1  3.7 item.treeStump
--, placeAt -5.0  4.0 item.woodenDoor

  , placeAt  1.0  0.0 item.helmet
--, placeAt -1.0  0.2 item.healthPotion
--, placeAt -3.0  1.2 item.dagger
--, placeAt -5.0  1.0 item.spear
--, placeAt  0.0  1.0 item.bag

  , placeAt -3.0 -1.0 item.bow
  -- , placeAt -3.0 -2.0 item.arrow
  -- , placeAt -3.1 -2.1 item.arrow
  -- , placeAt -3.2 -2.2 item.arrow
  -- , placeAt -3.3 -2.3 item.arrow
  -- , placeAt -3.7 -2.3 item.quiver

  , makeItem item.quiver
    [ actions.setLocation -3.7 -2.3
    , actions.addLoadout [ loadout.count 10 item.arrow ]
    ]
  ]

let makeUnit =
  λ(n : AgentTypeName) →
  λ(a : List EntityAction) →
    { name = n, actions = a }

let spawnAt =
  λ(x : Double) →
  λ(y : Double) →
  λ(n : AgentTypeName) →
    makeUnit n [ actions.setLocation x y ]

let units =
  [ spawnAt  11.0   8.0 agents.bat
  , spawnAt  10.0   8.3 agents.bat
  , spawnAt -11.0   8.0 agents.spider

  , makeUnit agents.npcBertram
    [ actions.setLocation 1.6 3.2
    , actions.addLoadout [ loadout.slot EquipmentSlot.Head item.helmet ]
    ]
  ]

in
{ size = [200, 200]
, seed = 29
, baseTileSet = tilesets.water.name
, baseLandTileSet = tilesets.dirtWet.name
, coveringLayers = [ grassLayer ]
, items = items
, units = units
}
