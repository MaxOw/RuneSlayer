let enums = ./Enums.dhall
let Direction     = enums.Direction
let AnimationKind = enums.AnimationKind

let Path = < MakePath : Text >

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

let UseActionEffect =
  < TransformInto  : { TransformInto  : PassiveTypeName }
  | InspectContent : { InspectContent : List {} }
  | DeleteSelf     : { DeleteSelf     : List {} }
  | Heal           : { Heal           : Natural }
  >

in
{ Path            = Path
, Rect            = Rect
, Sprite          = Sprite
, LocatedSprite   = LocatedSprite
, ContainerType   = ContainerType
, Frame           = Frame
, AnimationPart   = AnimationPart
, PassiveTypeName = PassiveTypeName
, UseActionEffect = UseActionEffect
}
