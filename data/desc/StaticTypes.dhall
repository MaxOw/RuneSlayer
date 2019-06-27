let types      = ./Types.dhall
let paths      = ./ResourcePaths.dhall
let sprites    = ./Sprites.dhall
let appearance = ./Appearance.dhall

let LocatedSprite  = types.LocatedSprite
let StaticTypeName = types.StaticTypeName

let makeName = StaticTypeName.MakeName

let defaultStaticType =
  {=} : {}

let makeSimple =
  λ(n : Text) →
  λ(a : List LocatedSprite) → defaultStaticType //
  { name = makeName n, appearance = a }

let treeAppearance =
  [ appearance.sprite  sprites.treeTrunk
  , appearance.located [0.0, 2.0] sprites.treeFoliage
  ]

let tree = makeSimple "Tree" treeAppearance

in
{ tree = tree
}
