let Rect   = { offset : List Natural, size : List Natural }
let Sprite =
  { name : Text
  , path : Text
  , part : Optional Rect
  , pixelsPerUnit : Optional Natural
  }

let ContainerType =
  { maxVolue : Double
  }

in
{ Rect          = Rect
, Sprite        = Sprite
, ContainerType = ContainerType
}
