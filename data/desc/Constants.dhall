let passives   = ./PassiveNames.dhall
let types      = ./Types.dhall
let enums      = ./Enums.dhall

let EquipmentSlot     = enums.EquipmentSlot
let Reactivity        = enums.Reactivity
let Entry             = types.Entry
let InteractionEffect = types.InteractionEffect
let InteractionEntry  = Entry Text (List InteractionEffect)

let defaultStats =
  { attack      = 0
  , attackRange = 0
  , defence     = 0
  , maxHealth   = 0
  , maxSpeed    = 0
  }

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
  , EquipmentSlot.SecondaryWeapon
  , EquipmentSlot.SecondaryOther
  ]

let defaultAgent =
  { animateWhenStopped = False
  , renderOffset       = None (List Double)
  , equipmentSlots     = [] : List EquipmentSlot
  , interactions       = [] : List InteractionEntry
  , primaryInteraction = None Text
  }

let defaultHumanAgent = defaultAgent //
  { corpse          = passives.humanCorpse
  , reactivity      = { Life = 0.1 }
  , hostileTowards  = [ Reactivity.Shadow ]
  , autoTargetRange = 8 -- meters

  , renderOffset    = [0.0, 0.8]
  , labelOffset     = [0.0, 1.7]

  , equipmentSlots  = defaultEquipmentSlots
  }

in
{ slowWalkingSpeed   = 1.0 -- m/s
, baseWalkingSpeed   = 2.0 -- m/s
, fastWalkingSpeed   = 3.0 -- m/s
, baseRunningSpeed   = 6.0 -- m/s
, baseSprintingSpeed = 8.0 -- m/s

, defaultStats          = defaultStats
, defaultEquipmentSlots = defaultEquipmentSlots
, defaultAgent          = defaultAgent
, defaultHumanAgent     = defaultHumanAgent
}
