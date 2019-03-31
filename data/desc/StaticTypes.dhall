let types      = ./Types.dhall
let paths      = ./ResourcePaths.dhall
let sprites    = ./Sprites.dhall
let appearance = ./Appearance.dhall

let LocatedSprite = types.LocatedSprite

let makeStatic =
  λ(n : Text) →
  λ(a : List LocatedSprite) →
  { name = n, appearance = a }

let treeAppearance =
  [ appearance.sprite  sprites.treeTrunk
  , appearance.located [0.0, 2.0] sprites.treeFoliage
  ]

in
{ tree = makeStatic "Tree" treeAppearance
}
