let constants  = ./Constants.dhall
let passives   = ./PassiveNames.dhall
let enums      = ./Enums.dhall
let animations = ./AnimationNames.dhall

let AgentKind = enums.AgentKind

let defaultBody =
  [ animations.maleBodyLight
  , animations.malePantsTeal
  , animations.maleShirtWhite
  , animations.maleHairPlainBrown
  ]

in constants.defaultHumanAgent //
{ name          = "Player"
, bodyAnimation = defaultBody

, stats = constants.defaultStats //
  { attack      = 1
  , attackRange = 2
  , defence     = 1
  , maxHealth   = 10
  , maxSpeed    = constants.fastWalkingSpeed
  }

, agentKind = AgentKind.Player
}
