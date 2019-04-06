let animations = ./Animations.dhall
let items      = ./ItemTypes.dhall
let enums      = ./Enums.dhall
let Reactivity = enums.Reactivity

in
{ bat =
  { name      = "Bat"
  , animation = animations.bat
  , maxHealth = 3
  , maxSpeed  = 2 -- meters per second
  , corpse    = Some items.batCorpse.name
  , animateWhenStopped = True
  , attackRange = 1  -- meter
  , attackPower = 2
  , attackSpeed = 1  -- seconds
  , aggroRange  = 5  -- meters
  , pursueRange = 10 -- meters
  , reactivity  = { Shadow = 1.0 }
  , hostileTowards = [ Reactivity.Life ]
  }
}
