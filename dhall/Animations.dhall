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
let makeCharAnim = λ(p : Path) →
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

let simpleCustomAnim =
  λ(frames : List Frame) →
    { CustomAnimation =
      [ { direction = Any Direction
        , kind      = Any AnimationKind
        , frames    = frames
        }
      ]
    }

let Indexed = λ(a : Type) → { index : Natural, value : a }

let defDirs = [Direction.North, Direction.West, Direction.South, Direction.East]

let overDirections =
  λ(dirs : List Direction) →
  λ(f : (Indexed Direction → AnimationPart)) →
    map (Indexed Direction) AnimationPart f (indexed Direction dirs)

let overEachDirection =
  λ(f : (Indexed Direction → AnimationPart)) →
    overDirections defDirs f

let makePartsOver =
  λ(dirs : List Direction) →
  λ(kind : Optional AnimationKind) →
  λ(s : Natural) →
  λ(x : Natural) →
  λ(y : Natural) →
  λ(n : Natural) →
  λ(sprite : Sprite) →
    overDirections dirs (λ(i : Indexed Direction) →
      { direction = Some (i.value)
      , kind      = kind
      , frames    = map Natural Frame
          (λ(o : Natural) → makeFrame s (x+o) (y+i.index) sprite)
          (enumerate n)
      }
    )

let makeParts = makePartsOver defDirs

let makePartsAnyDir =
  λ(kind : Optional AnimationKind) →
  λ(s : Natural) →
  λ(x : Natural) →
  λ(y : Natural) →
  λ(n : Natural) →
  λ(sprite : Sprite) → [
      { direction = Any Direction
      , kind      = kind
      , frames    = map Natural Frame
          (λ(o : Natural) → makeFrame s (x+o) y sprite)
          (enumerate n)
      }]

let makeSpiderAnim =
  λ(p : Path) →
    { CustomAnimation = concat AnimationPart
      [ makeParts       (Some AnimationKind.Walk)  64 4 0 6 (utils.makeSprite p)
      , makeParts       (Some AnimationKind.Slash) 64 0 0 4 (utils.makeSprite p)
      , makePartsAnyDir (Some AnimationKind.Die)   64 0 4 4 (utils.makeSprite p)
      ]
    }

let batDirs = [Direction.South, Direction.East, Direction.North, Direction.West]

let makeBatAnim =
  λ(p : Path) →
    { CustomAnimation = concat AnimationPart
      [ makePartsOver batDirs (Some AnimationKind.Die)  32 0 0 1 (utils.makeSprite p)
      , makePartsOver batDirs (Any  AnimationKind)      32 1 0 3 (utils.makeSprite p)
      ]
    }

let makeOversize =
  λ(p : Path) →
  λ(k : AnimationKind) →
    { CustomAnimation = concat AnimationPart
      [ makeParts (Some k) 192 0 0 7 (utils.makeSprite p)
      ]
    }

let oversizeSpear = makeOversize paths.oversizeSpearAnimation AnimationKind.Thrust
let oversizeSword = makeOversize paths.oversizeSwordAnimation AnimationKind.Slash

in
{ maleBodyLight      = makeCharAnim paths.maleBodyLight
, malePantsTeal      = makeCharAnim paths.malePantsTeal
, maleShirtWhite     = makeCharAnim paths.maleShirtWhite

, maleHairBangsLongBrown = makeCharAnim paths.maleHairBangsLongBrown
, maleHairPlainBrown     = makeCharAnim paths.maleHairPlainBrown
, maleBeardBrown         = makeCharAnim paths.maleBeardBrown

, helmet = makeCharAnim paths.helmetAnimation
, dagger = makeCharAnim paths.daggerAnimation
, bow    = makeCharAnim paths.bowAnimation
, arrow  = makeCharAnim paths.arrowAnimation
, quiver = makeCharAnim paths.quiverAnimation

, spear  = oversizeSpear -- makeCharAnim paths.spearAnimation
, sword  = oversizeSword

, bat = makeBatAnim paths.bat
, spider01 = makeSpiderAnim paths.spider01
}
