let constants  = ./Constants.dhall
let passives   = ./PassiveNames.dhall
let enums      = ./Enums.dhall
let animations = ./AnimationNames.dhall

let AgentKind     = enums.AgentKind
let EquipmentSlot = enums.EquipmentSlot

let defaultStats = constants.defaultStats

let defaultBody =
  [ animations.maleBodyLight
  , animations.malePantsTeal
  , animations.maleShirtWhite
  , animations.maleHairPlainBrown
  ]

let defaultEquipmentSlots =
  [ EquipmentSlot.Backpack
--, EquipmentSlot.Bundle
  , EquipmentSlot.Belt
  , EquipmentSlot.Head
  , EquipmentSlot.Torso
  , EquipmentSlot.Hands
  , EquipmentSlot.Legs
  , EquipmentSlot.Feet
  , EquipmentSlot.PrimaryWeapon
  , EquipmentSlot.PrimaryOther
  ]

in
{ name       = "Player"
, corpse     = passives.humanCorpse
, reactivity = constants.humanReactivity

, bodyAnimation      = defaultBody
, animateWhenStopped = False
, renderOffset       = [0.0, 0.8]

, stats = defaultStats //
  { attack    = 1
  , defence   = 1
  , maxHealth = 10
  , maxSpeed  = constants.fastWalkingSpeed
  }
, equipmentSlots = defaultEquipmentSlots

, agentKind = AgentKind.Player
}
