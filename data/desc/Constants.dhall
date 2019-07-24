let enums      = ./Enums.dhall

let EquipmentSlot = enums.EquipmentSlot

let defaultStats =
  { attack    = 0
  , defence   = 0
  , maxHealth = 0
  , maxSpeed  = 0
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
  ]

in
{ slowWalkingSpeed   = 1.0 -- m/s
, baseWalkingSpeed   = 2.0 -- m/s
, fastWalkingSpeed   = 3.0 -- m/s
, baseRunningSpeed   = 6.0 -- m/s
, baseSprintingSpeed = 8.0 -- m/s

, defaultStats          = defaultStats
, defaultEquipmentSlots = defaultEquipmentSlots

, humanReactivity = { Life = 0.1 }
}
