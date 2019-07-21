let PassiveKind =
  < Container
  | SmallItem
  | BigItem
  | Projectile
  | Arrow
  | Item
  >

let WeaponKind =
  < Slashing
  | Thrusting
  | Projecting
  >

let EquipmentSlot =
  < Backpack
  | Belt
  | Head
  | Torso
  | Hands
  | Legs
  | Feet
  | PrimaryWeapon
  | PrimaryOther
  >

let AgentKind =
  < Player
  | Enemy
  | NPC
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
{ PassiveKind   = PassiveKind
, WeaponKind    = WeaponKind
, EquipmentSlot = EquipmentSlot
, AgentKind     = AgentKind
, Direction     = Direction
, AnimationKind = AnimationKind
, Reactivity    = Reactivity
}
