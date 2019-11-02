
let standard =
  { name       = "LiberationSans"
  , regular    = "LiberationSans-Regular.ttf"
  , bold       = Some "LiberationSans-Bold.ttf"
  , italic     = Some "LiberationSans-Italic.ttf"
  , boldItalic = Some "LiberationSans-BoldItalic.ttf"
  }

let cjk =
  { name       = "SourceHanSerif"
  , regular    = "SourceHanSerif-Regular.otf"
  , bold       = None Text
  , italic     = None Text
  , boldItalic = None Text
  }

in
{ fontsPath = Some "data/fonts"
, fonts = [ standard, cjk ]
, defaultFont = standard.name
}
