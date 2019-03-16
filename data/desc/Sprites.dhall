let types = ./Types.dhall
let paths = ./ResourcePaths.dhall

let Rect = types.Rect

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
{ healthPotion = makeSprite "Health Potion" paths.healthPotion

, bag    = makeItem "Bag"    4 2
, helmet = makeItem "Helmet" 1 1

, treeTrunk   = makeEnv "Tree Trunk"   50 38 6 6
, treeFoliage = makeEnv "Tree Foliage" 48 24 6 6
}
