let Prelude = ./Prelude.dhall
let map = Prelude.`List`.map

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
  }

let EquipmentSlot =
   { Backpack      = "Backpack"
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

let Index = { index : Natural, value : Text }
let makeIndex = λ(i : Natural) → λ(v : Text) → { index = i, value = v }
let mapDirections =
  λ(b : Type) →
  λ(f : Index → b) →
    map Index b f
    [ makeIndex 0 Direction.North
    , makeIndex 1 Direction.West
    , makeIndex 2 Direction.South
    , makeIndex 3 Direction.East
    ]

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
