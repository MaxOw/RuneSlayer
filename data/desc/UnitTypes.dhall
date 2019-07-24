let animations = ./AnimationNames.dhall
let passives   = ./PassiveNames.dhall
let enums      = ./Enums.dhall
let constants  = ./Constants.dhall
let names      = ./AgentNames.dhall

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

let defaultAgent =
  { animateWhenStopped = False
  , renderOffset       = None (List Double)
  , equipmentSlots     = [] : List EquipmentSlot
  }

let defaultEnemyAgent = defaultAgent //
  { unitType  = defaultUnitType
  , agentKind = AgentKind.Enemy
  }

let defaultNPCAgent = defaultAgent //
  { agentKind = AgentKind.NPC
  }

let bat = defaultEnemyAgent //
  { name       = names.bat
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

let spider = defaultEnemyAgent //
  { name       = names.spider
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

let npcBertram = defaultNPCAgent //
  { name       = names.npcBertram
  , reactivity = constants.humanReactivity

  , bodyAnimation =
    [ animations.maleBodyLight
    , animations.malePantsTeal
    , animations.maleShirtWhite
    , animations.maleBeardBrown
    , animations.maleHairBangsLongBrown
    ]

  , stats = defaultStats //
    { attack    = 100
    , defence   = 100
    , maxHealth = 2000
    , maxSpeed  = constants.fastWalkingSpeed
    }
  , equipmentSlots = constants.defaultEquipmentSlots
  }

in
{ bat        = bat
, spider     = spider

, npcBertram = npcBertram
}
