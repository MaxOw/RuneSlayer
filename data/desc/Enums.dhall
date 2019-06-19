let WeaponKind =
  { Slashing   = "Slashing"
  , Thrusting  = "Thrusting"
  , Projecting = "Projecting"
  }

let ItemKind =
  { Container  = "Container"
  , SmallItem  = "SmallItem"
  , BigItem    = "BigItem"
  , Projectile = "Projectile"
  , Arrow      = "Arrow"
  }

let EquipmentSlot =
   { Backpack      = "Backpack"
   , Quiver        = "Quiver"
   , Belt          = "Belt"
   , Head          = "Head"
   , Torso         = "Torso"
   , Hands         = "Hands"
   , Legs          = "Legs"
   , Feet          = "Feet"
   , PrimaryWeapon = "PrimaryWeapon"
   , PrimaryOther  = "PrimaryOther"
   }

let Direction =
   { North = "North"
   , West  = "West"
   , South = "South"
   , East  = "East"
   }

let AnimationKind =
   { Cast   = "Cast"
   , Thrust = "Thrust"
   , Walk   = "Walk"
   , Slash  = "Slash"
   , Fire   = "Fire"
   , Die    = "Die"
   }

let Reactivity =
   { Shadow = "Shadow"
   , Light  = "Light"
   , Poison = "Poison"
   , Blood  = "Blood"
   , Life   = "Life"
   }

in
{ ItemKind      = ItemKind
, WeaponKind    = WeaponKind
, EquipmentSlot = EquipmentSlot
, Direction     = Direction
, AnimationKind = AnimationKind
, Reactivity    = Reactivity
}
