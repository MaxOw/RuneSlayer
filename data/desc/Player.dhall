let constants  = ./Constants.dhall
let passives   = ./PassiveNames.dhall
let enums      = ./Enums.dhall
let animations = ./AnimationNames.dhall

let AgentKind     = enums.AgentKind

let defaultBody =
  [ animations.maleBodyLight
  , animations.malePantsTeal
  , animations.maleShirtWhite
  , animations.maleHairPlainBrown
  ]

in
{ name       = "Player"
, corpse     = passives.humanCorpse
, reactivity = constants.humanReactivity

, bodyAnimation      = defaultBody
, animateWhenStopped = False
, renderOffset       = [0.0, 0.8]

, stats = constants.defaultStats //
  { attack    = 1
  , defence   = 1
  , maxHealth = 10
  , maxSpeed  = constants.fastWalkingSpeed
  }
, equipmentSlots = constants.defaultEquipmentSlots

, agentKind = AgentKind.Player
}
