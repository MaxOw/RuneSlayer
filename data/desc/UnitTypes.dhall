let animations = ./Animations.dhall
let items      = ./ItemTypes.dhall
let enums      = ./Enums.dhall
let constants  = ./Constants.dhall
let Reactivity = enums.Reactivity

let defaultEnemyUnit =
  { animateWhenStopped = False
  , renderOffset = None (List Double)
  , hostileTowards = [ Reactivity.Life ]
  }

in
{ bat = defaultEnemyUnit //
  { name      = "Bat"
  , animation = animations.bat
  , renderOffset = Some [0.0, 0.8]
  , maxHealth = 3
  , maxSpeed  = constants.baseWalkingSpeed
  , corpse    = Some items.batCorpse.name
  , animateWhenStopped = True
  , attackRange = 1  -- meter
  , attackPower = 2
  , attackSpeed = 1  -- seconds
  , aggroRange  = 5  -- meters
  , pursueRange = 10 -- meters
  , reactivity  = { Shadow = 1.0 }
  }

, spider = defaultEnemyUnit //
  { name      = "Spider"
  , animation = animations.spider01
  , maxHealth = 10
  , maxSpeed  = constants.slowWalkingSpeed
  , corpse    = Some items.spiderCorpse.name
  , attackRange = 1.2  -- meter
  , attackPower = 5
  , attackSpeed = 1  -- seconds
  , aggroRange  = 5  -- meters
  , pursueRange = 10 -- meters
  , reactivity  = { Shadow = 1.0 }
  }
}
