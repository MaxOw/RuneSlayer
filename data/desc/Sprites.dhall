let types = ./Types.dhall
let paths = ./ResourcePaths.dhall

let Rect = types.rect

let makePart =
  \(g : Natural) ->
  \(x : Natural) ->
  \(y : Natural) ->
    { offset = [ x*g, y*g ], size = [ g, g ] }

let makeRect =
  \(g : Natural) ->
  \(x : Natural) ->
  \(y : Natural) ->
  \(w : Natural) ->
  \(h : Natural) ->
    { offset = [ x*g, y*g ], size = [ w*g, h*g ] }

let makeSprite =
  \(name : Text) ->
  \(path : Text) ->
    { name = name, path = path, pixelsPerUnit = 32, part = None Rect }

let makeItem =
  \(name : Text) ->
  \(x : Natural) ->
  \(y : Natural) ->
    makeSprite name paths.itemsAtlas1 // { part = Some (makePart 32 x y) }

let makeEnv =
  \(name : Text) ->
  \(x : Natural) ->
  \(y : Natural) ->
  \(w : Natural) ->
  \(h : Natural) ->
    makeSprite name paths.envAtlas1 // { part = Some (makeRect 16 x y w h) }

in
{ healthPotion = makeSprite "Health Potion" paths.healthPotion

, bag    = makeItem "Bag"    4 2
, helmet = makeItem "Helmet" 1 1

, treeTrunk   = makeEnv "Tree Trunk"   50 38 6 6
, treeFoliage = makeEnv "Tree Foliage" 48 24 6 6
}
