let types  = ./Types.dhall
let Sprite = types.Sprite

let makeSprite =
  λ(s : Sprite) →
  { Sprite = s.name }

in
{ sprite = makeSprite
, empty  = None {}
}
