let Prelude = ./Prelude.dhall
let paths   = ./ResourcePaths.dhall
let sprites = ./Sprites.dhall
let utils   = ./Sprite/Utils.dhall
let types   = ./Types.dhall
let enums   = ./Enums.dhall

let map = Prelude.`List`.map

let Sprite = types.Sprite
let Frame  = types.Frame

let Direction     = enums.Direction
let AnimationKind = enums.AnimationKind

let Any = None Text
let makeCharAnimation = λ(p : Text) → { CharacterAnimation = p }

let selectSpritePart = utils.selectSpritePart

let simpleFrame = λ(s : Sprite) → { duration = 1.0, sprite = s }

let makeBatFrame =
  λ(n : Natural) →
    simpleFrame (selectSpritePart 32 n 0 sprites.bat)

in
{ maleBodyLight = makeCharAnimation paths.maleBodyLight

, bat =
  { CustomAnimation =
    [ { direction = Any
      , kind      = Any
      , frames    = map Natural Frame makeBatFrame [1, 2, 3]
      }
    ]
  }
}
