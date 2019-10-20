let paths     = ./ResourcePaths.dhall
let types     = ./Types.dhall
let utils     = ./Sprite/Utils.dhall
let collision = ./CollisionShape.dhall

let Edge =
  < Bottom
  | Left
  | Top
  | Right
  >

let Corner =
  < BottomRight
  | BottomLeft
  | TopLeft
  | TopRight
  >

let Cross =
  < TopLeftBottomRight
  | BottomLeftTopRight
  >

let TileRole =
  < Full        : { Full : List {} }
  | Edge        : { Edge : Edge }
  | OuterCorner : { OuterCorner : Corner }
  | InnerCorner : { InnerCorner : Corner }
  | Cross       : { Cross : Cross }
  >

let RoleFull        =                 TileRole.Full        { Full = [] : List  { } }
let RoleEdge        = λ(x : Edge)   → TileRole.Edge        { Edge = x }
let RoleOuterCorner = λ(x : Corner) → TileRole.OuterCorner { OuterCorner = x }
let RoleInnerCorner = λ(x : Corner) → TileRole.InnerCorner { InnerCorner = x }
let RoleCross       = λ(x : Cross)  → TileRole.Cross       { Cross = x }

let Path           = types.Path
let CollisionShape = types.CollisionShape
let CollideWith    = types.CollideWith
let CollisionEntry = { key : TileRole, value : CollisionShape }

let makeTileSet
  = λ(i : Natural)
  → λ(n : Text)
  → λ(p : Path)
  → { name            = n
    , color           = None Text
    , desc            = { Standard = utils.makeSprite p }
    , collisionShapes = [] : List CollisionEntry
    , collisionBits   = [] : List CollideWith
    , zindex          = i
    }

let mkEntry
  = λ(k : TileRole)
  → λ(v : CollisionShape)
  → { key = k, value = v }

let circle
  = λ(x : Double)
  → λ(y : Double)
  → λ(r : Double)
  → collision.translate x y (collision.circle r)

let rect
  = λ(x : Double)
  → λ(y : Double)
  → λ(w : Double)
  → λ(h : Double)
  → collision.translate x y (collision.rect w h)

let tileRolesCollisions =
  [ { key   = RoleEdge Edge.Top
    , value = rect  0.0  -0.25  1.0 0.5 }
  , { key   = RoleEdge Edge.Bottom
    , value = rect  0.0   0.25  1.0 0.5 }
  , { key   = RoleEdge Edge.Left
    , value = rect  0.25  0.0   0.5 1.0 }
  , { key   = RoleEdge Edge.Right
    , value = rect -0.25  0.0   0.5 1.0 }

  , { key   = RoleInnerCorner Corner.BottomRight
    , value = circle 0.0 0.0 0.5 }
  , { key   = RoleInnerCorner Corner.BottomLeft
    , value = circle 0.0 0.0 0.5 }
  , { key   = RoleInnerCorner Corner.TopLeft
    , value = circle 0.0 0.0 0.5 }
  , { key   = RoleInnerCorner Corner.TopRight
    , value = circle 0.0 0.0 0.5 }

  , { key   = RoleOuterCorner Corner.BottomRight
    , value = circle -0.5  0.5  0.5 }
  , { key   = RoleOuterCorner Corner.BottomLeft
    , value = circle  0.5  0.5  0.5 }
  , { key   = RoleOuterCorner Corner.TopLeft
    , value = circle  0.5 -0.5  0.5 }
  , { key   = RoleOuterCorner Corner.TopRight
    , value = circle -0.5 -0.5  0.5 }
  ]

in
{ dirtDry = makeTileSet 100 "Dry Dirt" paths.tilesetDirtDry
, dirtWet = makeTileSet 100 "Wet Dirt" paths.tilesetDirtWet
, water   = makeTileSet 400 "Water"    paths.tilesetWater //
    { collisionShapes = tileRolesCollisions
    , collisionBits   = [ CollideWith.Low ]
    }

, grass   = makeTileSet 600 "Grass"    paths.tilesetGrass //
    { color = Some "green" }
}
