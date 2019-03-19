let types = ../Types.dhall
let paths = ../ResourcePaths.dhall

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
  λ(name : Text) →
  λ(path : Text) →
    { name = name, path = path, pixelsPerUnit = Some 32, part = None Rect }

let selectSpritePart =
  λ(g : Natural) →
  λ(x : Natural) →
  λ(y : Natural) →
  λ(s : Sprite)  →
    s // { part = Some (makePart g x y) }

let makeItem =
  λ(name : Text) →
  λ(x : Natural) →
  λ(y : Natural) →
    makeSprite name paths.itemsAtlas1 // { part = Some (makePart 32 x y) }

let makeEnv =
  λ(name : Text) →
  λ(x : Natural) →
  λ(y : Natural) →
  λ(w : Natural) →
  λ(h : Natural) →
    makeSprite name paths.envAtlas1 // { part = Some (makeRect 16 x y w h) }

in
{ makePart         = makePart
, makeRect         = makeRect
, makeSprite       = makeSprite
, makeItem         = makeItem
, makeEnv          = makeEnv
, selectSpritePart = selectSpritePart
}
