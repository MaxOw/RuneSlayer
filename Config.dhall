
let DebugFlag =
  < NoTutorial
  | NoStory
  >

let InputMode =
  < NormalMode
  | InventoryMode
  | StoryDialogMode
  | RunicStatusMode
  | RunicMode
  | SpaceMode
  | StoryMode
  | MapEditorMode
  >

let MoveDirection = < MoveUp | MoveDown | MoveLeft | MoveRight >

let ToggleDebug =
  < DrawPickupRange
  | ZoomOutScroller
  | HideScroller
  | ShowDynamicBoundingBoxes
  | ShowCollisionShapes
  >

let Panel = < GroundPreviewPanel | StatusPanel >

let AnimationKind = < Cast | Thrust | Walk | Slash | Fire | Die >
let AttackMode = < Manual | Auto >

let MapEditorAction =
  < PlaceEntity
  | NextCategory
  | PrevCategory
  | SelectNext
  | SelectPrev
  >

let TutorialAction = < SkipAll | Restart >

let InputAction =
  < SetMode           : { _1 : InputMode }
  | SimpleMove        : { _1 : MoveDirection }
  | ToggleDebug       : { _1 : ToggleDebug }
  | DebugRunAnimation : { _1 : AnimationKind }
  | ToggleViewPanel   : { _1 : Panel }
  | PickupAllItems
  | DropAllItems
  | ExecuteAttack
  | SwapWeapon
  | SetAttackMode : { _1 : AttackMode }
  | StartRunicMode
  | SelectItemToPickUp
  | SelectItemToMove
  | SelectItemMoveTarget
  | SelectItemToDrop
  | SelectItemToFocus
  | UseFocusedItem
  | SelectInteraction
  | Interact
  | FastQuit
  | MapEditorAction : { _1 : MapEditorAction }
  | TutorialAction  : { _1 : TutorialAction }
  | NextPage
  | Escape
  >

--------------------------------------------------------------------------------

let KeymapEntry =
  { mode   : Optional InputMode
  , keyseq : Text
  , action : Optional InputAction
  }

-- map bindings for any mode
let mapAny
  = λ(s : Text)
  → λ(a : InputAction)
  → { mode = None InputMode, keyseq = s, action = Some a }

let map
  = λ(m : InputMode)
  → λ(s : Text)
  → λ(a : InputAction)
  → { mode = Some m, keyseq = s, action = Some a }

let nmap
  = λ(s : Text)
  → λ(a : InputAction)
  → map InputMode.NormalMode s a

let unmap
  = λ(m : InputMode)
  → λ(s : Text)
  → { mode = Some m, keyseq = s, action = None InputAction }

let bindings = [
  -- , nmap "<Up>"    (InputAction.SimpleMove { _1 = MoveDirection.MoveUp    })
  -- , nmap "<Down>"  (InputAction.SimpleMove { _1 = MoveDirection.MoveDown  })
  -- , nmap "<Left>"  (InputAction.SimpleMove { _1 = MoveDirection.MoveLeft  })
  -- , nmap "<Right>" (InputAction.SimpleMove { _1 = MoveDirection.MoveRight })

  -- , nmap "<Space>" (InputAction.ExecuteAttack)
  ] : List KeymapEntry

in
{ runeSet = "japanese"
-- , debugFlags = [DebugFlag.NoTutorial, DebugFlag.NoStory]
-- , debugFlags = [DebugFlag.NoStory]
, debugFlags = [] : List DebugFlag
, clearDefaultBindings = False
, bindings = bindings
}
