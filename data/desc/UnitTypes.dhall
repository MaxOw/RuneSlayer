let animations = ./Animations.dhall

in
{ bat =
  { name      = "Bat"
  , animation = animations.bat
  , maxHealth = 10
  }
}
