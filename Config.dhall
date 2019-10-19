
let DebugFlag =
  < NoTutorial
  | NoStory
  >

in
{ runeSet = "japanese"
, debugFlags = [DebugFlag.NoTutorial, DebugFlag.NoStory]
-- , debugFlags = [] : List DebugFlag
}
