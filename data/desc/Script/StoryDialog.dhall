let Prelude = ../Prelude.dhall
let enums   = ../Enums.dhall

let map = Prelude.List.map

let DialogName = enums.DialogName
let StoryDialog =
  { name        : DialogName
  , title       : Text
  , dialogPages : List Text
  }

-- Should probably take this from displayName in UnitType.dhall once that's in
let bertramDialogTitle = "Old Man Bertram"
let bertramWelcome =
  { name = DialogName.BertramWelcome
  , title = bertramDialogTitle
  , dialogPages =
    [ "This is first page, bla bla bla..."
    , "And this is second and last page!"
    ]
  }

let bertramWaiting =
  { name = DialogName.BertramWaiting
  , title = bertramDialogTitle
  , dialogPages =
    [ "What are you waiting for? Go kill some shadow spawn boy!"
    ]
  }

let StoryDialogEntry = { _1 : DialogName, _2 : StoryDialog }

let kv = λ(s : StoryDialog) → { _1 = s.name, _2 = s }

in map StoryDialog StoryDialogEntry kv
  [ bertramWelcome
  , bertramWaiting
  ]
