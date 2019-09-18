let enums    = ./Enums.dhall
let types    = ./Types.dhall
let item     = ./PassiveNames.dhall
let agent    = ./AgentNames.dhall
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

let baseChestLoadout
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

let trChestLoadout
  = λ(t : Type)
  → λ(fix : EntityActionF t → t)
  → let actions = actions.makeFix t fix
    let loadout = actions.loadout in
    actions.addLoadout
      [ loadout.count 3 item.healthPotion
      , loadout.containerWithCount item.quiver 15 item.arrow
      , loadout.containerWithCount item.quiver 15 item.arrow
      , loadout.simple item.helmet
      ]

let items =
  [ makeItem item.woodenChest
    [ actions.setLocation -0.3 5.3
    , baseChestLoadout
    ]

  , makeItem item.woodenChest
    [ actions.setLocation 5.5 31.0
    , trChestLoadout
    ]

  , placeAt -0.2  3.2 item.campfire
  , placeAt -2.0  2.0 item.firewood
  , placeAt -2.3  3.0 item.firewoodPileSmall
  , placeAt -2.1  4.8 item.barrel
  , placeAt -3.4  3.6 item.woodChoopingBlock
  , placeAt  2.1  3.7 item.treeStump

--, placeAt -3.0  1.2 item.dagger
--, placeAt -5.0  1.0 item.spear

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
  [ spawnAt  11.0   8.0 agent.bat
  , spawnAt  10.0   8.3 agent.bat
  , spawnAt -11.0   8.0 agent.spider
  , spawnAt -13.0   9.0 agent.spider

  , spawnAt  11.0  -8.0 agent.bat
  , spawnAt  10.0  -8.3 agent.bat

  , spawnAt -17.0  -8.0 agent.bat
  , spawnAt -18.0  -8.3 agent.bat

  , spawnAt 5.7 28.3 agent.bat
  , spawnAt 3.9 28.3 agent.bat
  , spawnAt 3.9 29.3 agent.bat
  , spawnAt -4.7 24.8 agent.bat
  , spawnAt -4.3 26.2 agent.bat
  , spawnAt -3.2 26.4 agent.bat
  , spawnAt -3.5 25.2 agent.bat
  , spawnAt -2.3 24.9 agent.bat
  , spawnAt -2.3 24.7 agent.bat
  , spawnAt -2.8 23.4 agent.bat
  , spawnAt 0.9 16.8 agent.bat
  , spawnAt 1.9 16.3 agent.bat
  , spawnAt 1.9 17.3 agent.bat
  , spawnAt 9.9 9.7 agent.bat
  , spawnAt 10.8 10.6 agent.bat
  , spawnAt 11.8 9.9 agent.bat
  , spawnAt 11.8 9.0 agent.bat
  , spawnAt 11.0 9.4 agent.bat

  , makeUnit agent.npcBertram
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
