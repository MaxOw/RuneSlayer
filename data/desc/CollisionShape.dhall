let types = ./Types.dhall

let BaseShape      = types.BaseShape
let CollisionShape = types.CollisionShape

-- Quick and dirty...
let CollisionShapeA =
  < Translate : { Translate : { vector : List Double, shapeDesc : CollisionShape } }
  >

let baseShape
  = λ(x : BaseShape)
  → CollisionShape.BaseShape { BaseShape = x }

let circle
  = λ(r : Double)
  → baseShape (BaseShape.Circle { Circle = r })

let rect
  = λ(w : Double)
  → λ(h : Double)
  → baseShape (BaseShape.Rect { Rect = { width = w, height = h } })

let translate
  = λ(x : Double)
  → λ(y : Double)
  → λ(s : CollisionShape)
  → CollisionShapeA.Translate
    { Translate = { vector = [x, y], shapeDesc = s } }

in
{ circle    = circle
, rect      = rect
, translate = translate
}
