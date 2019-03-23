let animations = ./Animations.dhall
let items      = ./ItemTypes.dhall

in
{ bat =
  { name      = "Bat"
  , animation = animations.bat
  , maxHealth = 3
  , corpse    = Some items.healthPotion.name
  }
}
