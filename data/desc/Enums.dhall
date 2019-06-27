let ItemKind =
  < Container
  | SmallItem
  | BigItem
  | Projectile
  | Arrow
  >

let WeaponKind =
  < Slashing
  | Thrusting
  | Projecting
  >

let EquipmentSlot =
  < Backpack
  | Quiver
  | Belt
  | Head
  | Torso
  | Hands
  | Legs
  | Feet
  | PrimaryWeapon
  | PrimaryOther
  >

let Direction =
  < North
  | West
  | South
  | East
  >

let AnimationKind =
  < Cast
  | Thrust
  | Walk
  | Slash
  | Fire
  | Die
  >

let Reactivity =
  < Shadow
  | Light
  | Poison
  | Blood
  | Life
  >

in
{ ItemKind      = ItemKind
, WeaponKind    = WeaponKind
, EquipmentSlot = EquipmentSlot
, Direction     = Direction
, AnimationKind = AnimationKind
, Reactivity    = Reactivity
}
