let types = ./Types.dhall

let Sprite        = types.Sprite
let LocatedSprite = types.LocatedSprite

let makeSprite =
  λ(s : Sprite) →
  { vector = [0.0, 0.0], value = s.name }

let makeLocated =
  λ(v : List Double) →
  λ(s : Sprite) →
  { vector = v, value = s.name }

let makeSimple = λ(s : Sprite) → [makeSprite s]

in
{ simple    = makeSimple
, sprite    = makeSprite
, located   = makeLocated
, empty     = [] : List LocatedSprite
}
