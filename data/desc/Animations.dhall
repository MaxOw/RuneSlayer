let Prelude = ./Prelude.dhall
let paths   = ./ResourcePaths.dhall
let sprites = ./Sprites.dhall
let utils   = ./Sprite/Utils.dhall
let types   = ./Types.dhall
let enums   = ./Enums.dhall

let map       = Prelude.List.map
let indexed   = Prelude.List.indexed
let concat    = Prelude.List.concat
let enumerate = Prelude.Natural.enumerate

let Path          = types.Path
let Sprite        = types.Sprite
let Frame         = types.Frame
let AnimationPart = types.AnimationPart

let Direction     = enums.Direction
let AnimationKind = enums.AnimationKind

let Any = None
let makeCharAnimation = λ(p : Path) →
  { CharacterAnimation = utils.makeSprite p }

let selectSpritePart = utils.selectSpritePart

let simpleFrame = λ(s : Sprite) → { duration = 1.0, sprite = s }

let makeBatFrame =
  λ(n : Natural) →
    simpleFrame (selectSpritePart 32 n 0 sprites.bat)

let makeFrame =
  λ(s : Natural) →
  λ(x : Natural) →
  λ(y : Natural) →
  λ(sprite : Sprite) →
    simpleFrame (selectSpritePart s x y sprite)

let simpleCustomAnimation =
  λ(frames : List Frame) →
    { CustomAnimation =
      [ { direction = Any Direction
        , kind      = Any AnimationKind
        , frames    = frames
        }
      ]
    }

let Indexed = λ(a : Type) → { index : Natural, value : a }

let overEachDirection =
  λ(f : (Indexed Direction → AnimationPart)) →
    map (Indexed Direction) AnimationPart f
      (indexed Direction
        [Direction.North, Direction.West, Direction.South, Direction.East]
      )

let makeParts =
  λ(kind : Optional AnimationKind) →
  λ(s : Natural) →
  λ(x : Natural) →
  λ(y : Natural) →
  λ(n : Natural) →
  λ(sprite : Sprite) →
    overEachDirection (λ(i : Indexed Direction) →
      { direction = Some (i.value)
      , kind      = kind
      , frames    = map Natural Frame
          (λ(o : Natural) → makeFrame s (x+o) (y+i.index) sprite)
          (enumerate n)
      }
    )

let makeSpiderAnimation =
  λ(p : Path) →
    { CustomAnimation = concat AnimationPart
      [ makeParts (Some AnimationKind.Walk)  64 4 0 6 (utils.makeSprite p)
      , makeParts (Some AnimationKind.Slash) 64 0 0 4 (utils.makeSprite p)
      ]
    }

in
{ maleBodyLight      = makeCharAnimation paths.maleBodyLight
, maleHairPlainBrown = makeCharAnimation paths.maleHairPlainBrown
, malePantsTeal      = makeCharAnimation paths.malePantsTeal
, maleShirtWhite     = makeCharAnimation paths.maleShirtWhite

, helmet = makeCharAnimation paths.helmetAnimation
, dagger = makeCharAnimation paths.daggerAnimation
, spear  = makeCharAnimation paths.spearAnimation
, bow    = makeCharAnimation paths.bowAnimation
, arrow  = makeCharAnimation paths.arrowAnimation
, quiver = makeCharAnimation paths.quiverAnimation

, bat = simpleCustomAnimation (map Natural Frame makeBatFrame [1, 2, 3])
, spider01 = makeSpiderAnimation paths.spider01
}
