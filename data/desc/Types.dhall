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

let ItemTypeName = < MakeName : Text >

let ItemUseEffect =
  < TransformInto : { TransformInto : ItemTypeName }
  | Heal          : { Heal          : Natural }
  >

let StaticTypeName = < MakeName : Text >

in
{ Path           = Path
, Rect           = Rect
, Sprite         = Sprite
, LocatedSprite  = LocatedSprite
, ContainerType  = ContainerType
, Frame          = Frame
, AnimationPart  = AnimationPart
, ItemTypeName   = ItemTypeName
, ItemUseEffect  = ItemUseEffect
, StaticTypeName = StaticTypeName
}
