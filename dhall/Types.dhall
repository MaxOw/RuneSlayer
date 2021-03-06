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

let PassiveTypeName = < MakePassiveTypeName : Text >
let AgentTypeName   = < MakeAgentTypeName   : Text >
let AnimationName   = < MakeAnimationName   : Text >
let Probability     = < MakeProbability     : Double >

let InteractionEffect =
  < TransformInto  : { TransformInto  : PassiveTypeName }
  | InspectContent : { InspectContent : List {} }
  | DeleteSelf     : { DeleteSelf     : List {} }
  | Heal           : { Heal           : Natural }
  | TalkTo         : { TalkTo         : List {} }
  >

let Range = { rangeMin : Natural, rangeMax : Natural }

let Spawn
  = λ(n : Type)
  → λ(t : Type)
  → { name    : n
    , actions : List t
    }

let SelectionEntry
  = λ(t : Type) →
  { probability : Optional Probability
  , entry       : Spawn PassiveTypeName t
  }

let LoadoutEntry
  = λ(t : Type) →
  { probability : Optional Probability
  , slot        : Optional EquipmentSlot
  , countRange  : Optional Range
  , selection   : List (SelectionEntry t)
  }

let BaseShape =
  < Circle : { Circle : Double }
  | Rect   : { Rect : { width : Double, height : Double } }
  >

let CollisionShape =
  < BaseShape : { BaseShape : BaseShape }
  >

let CollideWith = < High | Low >

let LocationP = { x : Double, y : Double }

let EntityValue =
  < Location  : { _1 : LocationP }
  | Direction : { _1 : Direction }
  >

let EntityActionF
  = λ(t : Type) →
  < SetValueF   : { _1 : EntityValue }
  | AddLoadoutF : { _1 : List (LoadoutEntry t) }
  >

let EntityAction
  = ∀(t : Type)
  → ∀(fix : EntityActionF t → t)
  → t

in
{ Entry             = Entry
, Path              = Path
, Location          = Location
, Rect              = Rect
, Sprite            = Sprite
, LocatedSprite     = LocatedSprite
, ContainerType     = ContainerType
, Frame             = Frame
, AnimationPart     = AnimationPart
, PassiveTypeName   = PassiveTypeName
, AgentTypeName     = AgentTypeName
, AnimationName     = AnimationName
, Probability       = Probability
, InteractionEffect = InteractionEffect
, Range             = Range
, Spawn             = Spawn
, SelectionEntry    = SelectionEntry
, LoadoutEntry      = LoadoutEntry
, EntityValue       = EntityValue
, EntityActionF     = EntityActionF
, EntityAction      = EntityAction
, BaseShape         = BaseShape
, CollisionShape    = CollisionShape
, CollideWith       = CollideWith
}
