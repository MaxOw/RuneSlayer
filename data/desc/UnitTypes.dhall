let animations = ./AnimationNames.dhall
let passives   = ./PassiveNames.dhall
let enums      = ./Enums.dhall
let constants  = ./Constants.dhall

let AgentKind     = enums.AgentKind
let EquipmentSlot = enums.EquipmentSlot
let Reactivity    = enums.Reactivity

let defaultStats = constants.defaultStats

let defaultUnitType =
  { attackRange = 0 -- meter
  , attackSpeed = 0 -- seconds
  , aggroRange  = 0 -- meters
  , pursueRange = 0 -- meters
  , hostileTowards = [ Reactivity.Life ]
  }

let defaultEnemyAgent =
  { animateWhenStopped = False
  , renderOffset = None (List Double)
  , unitType = defaultUnitType
  , agentKind = AgentKind.Enemy
  , equipmentSlots = [] : List EquipmentSlot
  }

in
{ bat = defaultEnemyAgent //
  { name       = "Bat"
  , corpse     = passives.batCorpse
  , reactivity = { Shadow = 1.0 }

  , bodyAnimation      = [ animations.bat ]
  , animateWhenStopped = True

  , stats = defaultStats //
    { attack    = 2
    , maxHealth = 3
    , maxSpeed  = constants.baseWalkingSpeed
    }

  , unitType = defaultUnitType //
    { attackRange = 1  -- meter
    , attackSpeed = 1  -- seconds
    , aggroRange  = 5  -- meters
    , pursueRange = 10 -- meters
    }

  , renderOffset = Some [0.0, 0.8]
  }

, spider = defaultEnemyAgent //
  { name       = "Spider"
  , corpse     = passives.spiderCorpse
  , reactivity = { Shadow = 1.0 }

  , bodyAnimation = [ animations.spider01 ]

  , stats = defaultStats //
    { attack    = 5
    , maxHealth = 10
    , maxSpeed  = constants.slowWalkingSpeed
    }

  , unitType = defaultUnitType //
    { attackRange = 1.2 -- meter
    , attackSpeed = 1   -- seconds
    , aggroRange  = 5   -- meters
    , pursueRange = 10  -- meters
    }
  }
}
