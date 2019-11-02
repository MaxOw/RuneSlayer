let types = ../Types.dhall
let paths = ../ResourcePaths.dhall

let Path   = types.Path
let Rect   = types.Rect
let Sprite = types.Sprite

let makePart =
  λ(g : Natural) →
  λ(x : Natural) →
  λ(y : Natural) →
    { offset = [ x*g, y*g ], size = [ g, g ] }

let makeRect =
  λ(g : Natural) →
  λ(x : Natural) →
  λ(y : Natural) →
  λ(w : Natural) →
  λ(h : Natural) →
    { offset = [ x*g, y*g ], size = [ w*g, h*g ] }

let makeSprite =
  λ(path : Path) →
    { path = path, pixelsPerUnit = Some 32, part = None Rect }

let selectSpritePart =
  λ(g : Natural) →
  λ(x : Natural) →
  λ(y : Natural) →
  λ(s : Sprite)  →
    s // { part = Some (makePart g x y) }

let makeItem =
  λ(x : Natural) →
  λ(y : Natural) →
    makeSprite paths.itemsAtlas1 // { part = Some (makePart 32 x y) }

let makeEnv =
  λ(x : Natural) →
  λ(y : Natural) →
  λ(w : Natural) →
  λ(h : Natural) →
    makeSprite paths.envAtlas1 // { part = Some (makeRect 16 x y w h) }

let makeEnv2 =
  λ(x : Natural) →
  λ(y : Natural) →
  λ(w : Natural) →
  λ(h : Natural) →
    makeSprite paths.envAtlas2 // { part = Some (makeRect 32 x y w h) }

let makeDoor =
  λ(x : Natural) →
  λ(y : Natural) →
    makeSprite paths.doors // { part = Some (makeRect 32 x y 2 2) }

in
{ makePart         = makePart
, makeRect         = makeRect
, makeSprite       = makeSprite
, makeItem         = makeItem
, makeEnv          = makeEnv
, makeEnv2         = makeEnv2
, makeDoor         = makeDoor
, selectSpritePart = selectSpritePart
}
