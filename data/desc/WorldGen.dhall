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

let chestLoadoutStart
  = λ(t : Type)
  → λ(fix : EntityActionF t → t)
  → let actions = actions.makeFix t fix
    let loadout = actions.loadout in
    actions.addLoadout
      [ loadout.simple item.bag
      , loadout.count 3 item.healthPotion
      , loadout.containerWithCount item.quiver 15 item.arrow
      , loadout.containerWithCount item.quiver 15 item.arrow
      , loadout.containerWithCount item.quiver 15 item.arrow
      ]

let chestLoadout0
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

let chestLoadout1
  = λ(t : Type)
  → λ(fix : EntityActionF t → t)
  → let actions = actions.makeFix t fix
    let loadout = actions.loadout in
    actions.addLoadout
      [ loadout.count 2 item.healthPotion
      , loadout.containerWithCount item.quiver 15 item.arrow
      , loadout.simple item.dagger
      ]

let chestLoadout2
  = λ(t : Type)
  → λ(fix : EntityActionF t → t)
  → let actions = actions.makeFix t fix
    let loadout = actions.loadout in
    actions.addLoadout
      [ loadout.count 2 item.healthPotion
      , loadout.containerWithCount item.quiver 15 item.arrow
      , loadout.simple item.sword
      ]

let chestLoadout3
  = λ(t : Type)
  → λ(fix : EntityActionF t → t)
  → let actions = actions.makeFix t fix
    let loadout = actions.loadout in
    actions.addLoadout
      [ loadout.count 2 item.healthPotion
      , loadout.containerWithCount item.quiver 15 item.arrow
      , loadout.simple item.spear
      ]

let placeChest
  = λ(x : Double)
  → λ(y : Double)
  → λ(a : EntityAction)
  → makeItem item.woodenChest [ actions.setLocation x y, a ]

let placeFullQuiver
  = λ(x : Double)
  → λ(y : Double)
  → makeItem item.quiver
    [ actions.setLocation x y
    , actions.addLoadoutCount 15 item.arrow
    ]

let items =
  [ placeChest  -0.3   5.3 chestLoadoutStart
  , placeChest   5.5  31.0 chestLoadout0
  , placeChest -32.6 -30.6 chestLoadout1
  , placeChest -40.0  17.9 chestLoadout2
  , placeChest -14.7  15.9 chestLoadout3

  , placeAt -0.2  3.2 item.campfire
  , placeAt -2.0  2.0 item.firewood
  , placeAt -2.3  3.0 item.firewoodPileSmall
  , placeAt -2.1  4.8 item.barrel
  , placeAt -3.4  3.6 item.woodChoopingBlock
  , placeAt  2.1  3.7 item.treeStump

  , placeFullQuiver 24.3 -5.1
  , placeFullQuiver -21.5 -18.9
  , placeFullQuiver -6.6 -23.1
  , placeAt 2.0 -29.1 item.healthPotion
  , placeAt 20.3 -43.0 item.barrel
  , placeAt 21.2 -44.3 item.healthPotion
  , placeFullQuiver 20.1 -43.8
  , placeFullQuiver 4.7 -42.4
  , placeAt -9.5 -60.9 item.healthPotion
  , placeFullQuiver -21.8 -49.9
  , placeFullQuiver -39.3 -36.9
  , placeAt -11.3 9.9 item.healthPotion


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

let units0 = [ spawnAt  11.0   8.0 agent.bat ]
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
  , spawnAt 9.7 1.4 agent.bat
  , spawnAt 12.5 1.4 agent.bat
  , spawnAt 17.2 4.4 agent.bat
  , spawnAt 18.0 6.1 agent.bat
  , spawnAt 20.5 12.2 agent.bat
  , spawnAt 26.3 20.1 agent.bat
  , spawnAt 27.5 12.8 agent.bat
  , spawnAt 26.5 10.0 agent.bat
  , spawnAt 27.4 8.4 agent.bat
  , spawnAt 30.7 4.5 agent.bat
  , spawnAt 31.4 1.2 agent.bat
  , spawnAt 23.7 -7.3 agent.bat
  , spawnAt 23.7 -5.8 agent.bat
  , spawnAt 24.7 -5.1 agent.bat
  , spawnAt -22.5 -19.6 agent.bat
  , spawnAt -22.6 -18.5 agent.bat
  , spawnAt -21.2 -18.5 agent.bat
  , spawnAt -13.4 -15.8 agent.bat
  , spawnAt -9.4 -11.8 agent.bat
  , spawnAt -1.9 -11.3 agent.bat
  , spawnAt 2.6 -13.1 agent.bat
  , spawnAt 4.5 -14.9 agent.bat
  , spawnAt 3.6 -20.1 agent.bat
  , spawnAt 3.2 -20.3 agent.bat
  , spawnAt 2.0 -21.1 agent.bat
  , spawnAt -5.3 -24.0 agent.bat
  , spawnAt -6.4 -23.1 agent.bat
  , spawnAt -6.6 -23.1 agent.bat
  , spawnAt 4.6 -31.3 agent.bat
  , spawnAt 3.3 -32.7 agent.bat
  , spawnAt 3.3 -31.0 agent.bat
  , spawnAt 4.4 -29.9 agent.bat
  , spawnAt 9.7 -29.8 agent.bat
  , spawnAt 14.6 -33.1 agent.bat
  , spawnAt 13.8 -34.2 agent.bat
  , spawnAt 14.6 -34.8 agent.bat
  , spawnAt 16.5 -42.4 agent.bat
  , spawnAt 17.4 -41.6 agent.bat
  , spawnAt 18.9 -40.6 agent.bat
  , spawnAt 20.2 -40.6 agent.bat
  , spawnAt 21.0 -42.0 agent.bat
  , spawnAt 19.8 -42.0 agent.bat
  , spawnAt 18.5 -42.0 agent.bat
  , spawnAt 18.5 -43.9 agent.bat
  , spawnAt 4.3 -45.2 agent.bat
  , spawnAt 2.4 -44.4 agent.bat
  , spawnAt 3.5 -43.7 agent.bat
  , spawnAt -7.4 -50.8 agent.bat
  , spawnAt -5.5 -50.8 agent.bat
  , spawnAt -4.1 -52.7 agent.bat
  , spawnAt -8.1 -60.7 agent.bat
  , spawnAt -9.0 -59.7 agent.bat
  , spawnAt -10.8 -60.6 agent.bat
  , spawnAt -19.3 -50.5 agent.bat
  , spawnAt -21.0 -49.6 agent.bat
  , spawnAt -23.2 -51.0 agent.bat
  , spawnAt -32.5 -52.5 agent.bat
  , spawnAt -33.0 -51.4 agent.bat
  , spawnAt -34.4 -51.4 agent.bat
  , spawnAt -38.7 -47.1 agent.bat
  , spawnAt -39.9 -46.2 agent.bat
  , spawnAt -50.1 -34.2 agent.bat
  , spawnAt -44.4 -29.4 agent.bat
  , spawnAt -44.2 -30.5 agent.bat
  , spawnAt -38.6 -36.2 agent.bat
  , spawnAt -37.7 -37.9 agent.bat
  , spawnAt -33.6 -29.4 agent.bat
  , spawnAt -31.1 -30.3 agent.bat
  , spawnAt -31.9 -28.9 agent.bat
  , spawnAt -30.5 -26.9 agent.bat
  , spawnAt -32.6 -27.6 agent.bat
  , spawnAt -30.9 -27.9 agent.bat
  , spawnAt -29.7 -27.9 agent.bat
  , spawnAt -29.7 -29.0 agent.bat
  , spawnAt -36.2 -23.3 agent.bat
  , spawnAt -34.9 -22.3 agent.bat
  , spawnAt -35.8 -21.3 agent.bat
  , spawnAt -28.8 -16.5 agent.bat
  , spawnAt -28.7 -9.3 agent.bat
  , spawnAt -25.4 -6.0 agent.bat
  , spawnAt -26.6 -5.7 agent.bat
  , spawnAt -25.2 -4.8 agent.bat
  , spawnAt -18.9 3.1 agent.spider
  , spawnAt -22.6 3.9 agent.spider
  , spawnAt -31.9 24.3 agent.spider
  , spawnAt -30.8 28.4 agent.spider
  , spawnAt -36.8 33.1 agent.spider
  , spawnAt -34.9 32.7 agent.spider
  , spawnAt -34.9 34.4 agent.spider
  , spawnAt -34.3 40.1 agent.bat
  , spawnAt -33.5 38.9 agent.bat
  , spawnAt -32.6 39.9 agent.bat
  , spawnAt -28.0 44.2 agent.bat
  , spawnAt -29.0 45.4 agent.bat
  , spawnAt -27.5 46.4 agent.bat
  , spawnAt -25.0 45.9 agent.bat
  , spawnAt -21.7 43.5 agent.bat
  , spawnAt -16.9 40.4 agent.bat
  , spawnAt -12.3 40.0 agent.bat
  , spawnAt -16.1 38.5 agent.bat
  , spawnAt -18.1 39.0 agent.bat
  , spawnAt -15.9 31.7 agent.spider
  , spawnAt -14.5 24.7 agent.spider
  , spawnAt -23.2 27.7 agent.spider
  , spawnAt -22.3 26.1 agent.spider
  , spawnAt -24.9 25.4 agent.spider
  , spawnAt -37.4 19.2 agent.bat
  , spawnAt -38.1 18.0 agent.bat
  , spawnAt -36.9 17.5 agent.bat
  , spawnAt -37.6 16.4 agent.bat
  , spawnAt -38.4 15.4 agent.bat
  , spawnAt -31.0 16.9 agent.bat
  , spawnAt -29.9 15.7 agent.bat
  , spawnAt -30.9 15.1 agent.bat
  , spawnAt -28.2 10.7 agent.bat
  , spawnAt -27.1 10.7 agent.bat
  , spawnAt -29.6 10.1 agent.bat
  , spawnAt -30.6 8.8 agent.bat
  , spawnAt -29.0 8.1 agent.bat
  , spawnAt -27.4 8.1 agent.bat
  , spawnAt -27.4 9.1 agent.bat
  , spawnAt -26.1 9.1 agent.bat
  , spawnAt -16.7 15.8 agent.spider
  , spawnAt -15.1 17.8 agent.spider
  , spawnAt -12.9 16.7 agent.spider
  , spawnAt -9.3 10.2 agent.spider

  , makeUnit agent.npcBertram
    [ actions.setLocation 1.2 3.6
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
, startLocation = { x = 1.2, y = 0.0 }
}
