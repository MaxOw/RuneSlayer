let enums    = ./Enums.dhall
let types    = ./Types.dhall
let item     = ./PassiveNames.dhall
let agents   = ./AgentNames.dhall
let tilesets = ./TileSets.dhall
let actions  = ./EntityActions.dhall

let PassiveTypeName = types.PassiveTypeName
let AgentTypeName   = types.AgentTypeName
let EntityActionF   = types.EntityActionF
let EntityAction    = types.EntityAction

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

let chestLoadout
  = λ(t : Type)
  → λ(fix : EntityActionF t → t)
  → let actions = actions.makeFix t fix
    let loadout = actions.loadout in
    actions.addLoadout
      [ loadout.simple item.bag
      , loadout.count 3 item.healthPotion
      , loadout.containerWithCount item.quiver 15 item.arrow
      , loadout.containerWithCount item.quiver 15 item.arrow
      ]

let items =
  [ makeItem item.woodenChest
    [ actions.setLocation -0.3 5.3
    , chestLoadout
    ]

  , placeAt -0.2  3.2 item.campfire
  , placeAt -2.0  2.0 item.firewood
  , placeAt -2.3  3.0 item.firewoodPileSmall
  , placeAt -2.1  4.8 item.barrel
  , placeAt -3.4  3.6 item.woodChoopingBlock
  , placeAt  2.1  3.7 item.treeStump
--, placeAt -5.0  4.0 item.woodenDoor

--, placeAt  1.0  0.0 item.helmet
--, placeAt -1.0  0.2 item.healthPotion
--, placeAt -3.0  1.2 item.dagger
--, placeAt -5.0  1.0 item.spear
--, placeAt  0.0  1.0 item.bag

  , placeAt  2.4  0.8 item.bow
  -- , placeAt -3.0 -2.0 item.arrow
  -- , placeAt -3.1 -2.1 item.arrow
  -- , placeAt -3.2 -2.2 item.arrow
  -- , placeAt -3.3 -2.3 item.arrow
  -- , placeAt -3.7 -2.3 item.quiver

  {-
  , placeAt  2.3  4.8 item.spear
  , placeAt -1.3  0.8 item.sword

  , makeItem item.quiver
    [ actions.setLocation -3.7 -2.3
    , actions.addLoadoutCount 8 item.arrow
    ]
  -}
  ]


let makeUnit =
  λ(n : AgentTypeName) →
  λ(a : List EntityAction) →
    { name = n, actions = a }

let spawnAt
  = λ(x : Double)
  → λ(y : Double)
  → λ(n : AgentTypeName)
  → makeUnit n [ actions.setLocation x y ]

let npcBertramLoadout
  = λ(t : Type)
  → λ(fix : EntityActionF t → t)
  → let actions = actions.makeFix t fix
    let loadout = actions.loadout in
    actions.addLoadout
      [ loadout.simple item.sword
      -- [ loadout.simple item.helmet
      -- , loadout.simpleWith item.quiver
        -- [ actions.addLoadout [ loadout.count 9 item.arrow ]
        -- ]
      ]

let units =
  [ spawnAt  11.0   8.0 agents.bat
  , spawnAt  10.0   8.3 agents.bat
  , spawnAt -11.0   8.0 agents.spider
  , spawnAt -13.0   9.0 agents.spider

  , spawnAt  11.0  -8.0 agents.bat
  , spawnAt  10.0  -8.3 agents.bat

  , spawnAt -17.0  -8.0 agents.bat
  , spawnAt -18.0  -8.3 agents.bat

  , makeUnit agents.npcBertram
    [ actions.setLocation 1.2 2.6
    , npcBertramLoadout
    ]
  ]

in
{ size = { width = 200.0, height = 200.0 }
, seed = +29
, baseTileSet = Some tilesets.water.name
, baseLandTileSet = Some tilesets.dirtWet.name
, coveringLayers = [ grassLayer ]
, items = items
, units = units
}
