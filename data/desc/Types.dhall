let Rect   = { offset : List Natural, size : List Natural }
let Sprite =
  { path : Text
  , part : Optional Rect
  , pixelsPerUnit : Optional Natural
  }

let LocatedSprite =
  { vector : List Double
  , value  : Sprite
  }

let ContainerType =
  { maxVolue : Double
  }

let Frame =
  { duration  : Double
  , sprite    : Sprite
  }

let AnimationPart =
  { direction : Optional Text
  , kind      : Optional Text
  , frames    : List Frame
  }

in
{ Rect          = Rect
, Sprite        = Sprite
, LocatedSprite = LocatedSprite
, ContainerType = ContainerType
, Frame         = Frame
, AnimationPart = AnimationPart
}
