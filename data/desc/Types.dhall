let enums = ./Enums.dhall

let Direction     = enums.Direction
let AnimationKind = enums.AnimationKind
let EquipmentSlot = enums.EquipmentSlot

let Entry = ./PreludeEntryType.dhall

let Path = < MakePath : Text >

let Location = List Double

let Rect   = { offset : List Natural, size : List Natural }
let Sprite =
  { path : Path
  , part : Optional Rect
  , pixelsPerUnit : Optional Natural
  }

let LocatedSprite =
  { vector : List Double
  , value  : Sprite
  }

let ContainerType =
  { maxVolue   : Double
  , allowKinds : List Text
  }

let Frame =
  { duration  : Double
  , sprite    : Sprite
  }

let AnimationPart =
  { direction : Optional Direction
  , kind      : Optional AnimationKind
  , frames    : List Frame
  }

let PassiveTypeName = < MakeName : Text >
let AgentTypeName   = < MakeName : Text >
let AnimationName   = < MakeName : Text >

let UseActionEffect =
  < TransformInto  : { TransformInto  : PassiveTypeName }
  | InspectContent : { InspectContent : List {} }
  | DeleteSelf     : { DeleteSelf     : List {} }
  | Heal           : { Heal           : Natural }
  >

let SelectionEntry =
  λ(a : Type) →
  { probability : Optional Double, name : a }

let LoadoutEntry =
  { probability  : Optional Double
  , slot         : Optional EquipmentSlot
  , countRange   : Optional (List Natural) -- TODO: Make a Range Type
  , selection    : List (SelectionEntry PassiveTypeName)
  }

let EntityValue =
  < Location       : { Location       : Location }
  >

let EntityAction =
  < SetValue       : { SetValue       : EntityValue }
  | AddLoadout     : { AddLoadout     : List LoadoutEntry }
  >

in
{ Entry           = Entry
, Path            = Path
, Location        = Location
, Rect            = Rect
, Sprite          = Sprite
, LocatedSprite   = LocatedSprite
, ContainerType   = ContainerType
, Frame           = Frame
, AnimationPart   = AnimationPart
, PassiveTypeName = PassiveTypeName
, AgentTypeName   = AgentTypeName
, AnimationName   = AnimationName
, UseActionEffect = UseActionEffect
, SelectionEntry  = SelectionEntry
, LoadoutEntry    = LoadoutEntry
, EntityValue     = EntityValue
, EntityAction    = EntityAction
}
