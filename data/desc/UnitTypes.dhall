let animations  = ./AnimationNames.dhall
let passives    = ./PassiveNames.dhall
let types       = ./Types.dhall
let enums       = ./Enums.dhall
let constants   = ./Constants.dhall
let names       = ./AgentNames.dhall
let interaction = ./InteractionEffect.dhall

let Entry             = types.Entry
let AgentKind         = enums.AgentKind
let ScriptName        = enums.ScriptName
let EquipmentSlot     = enums.EquipmentSlot
let Reactivity        = enums.Reactivity
let InteractionEffect = types.InteractionEffect
let InteractionEntry  = Entry Text (List InteractionEffect)

let defaultStats = constants.defaultStats

let defaultUnitType =
  { attackSpeed = 0 -- seconds
  , pursueRange = 0 -- meters
  }

let defaultEnemyAgent = constants.defaultAgent //
  { unitType  = defaultUnitType
  , agentKind = AgentKind.Enemy
  , hostileTowards = [ Reactivity.Life ]
  }

let defaultHumanNPCAgent = constants.defaultHumanAgent //
  { agentKind = AgentKind.NPC
  }

let bat = defaultEnemyAgent //
  { name       = names.bat
  , corpse     = passives.batCorpse
  , reactivity = { Shadow = 1.0 }

  , autoTargetRange = 5 -- meters

  , bodyAnimation      = [ animations.bat ]
  , animateWhenStopped = True

  , stats = defaultStats //
    { attack      = 2
    , attackRange = 1 -- meter
    , maxHealth   = 3
    , maxSpeed    = constants.baseWalkingSpeed
    }

  , unitType = defaultUnitType //
    { attackSpeed = 1  -- seconds
    , pursueRange = 10 -- meters
    }

  , renderOffset = [0.0, 0.8]
  , labelOffset  = [0.0, 1.9]
  }

let spider = defaultEnemyAgent //
  { name       = names.spider
  , corpse     = passives.spiderCorpse
  , reactivity = { Shadow = 1.0 }
  , autoTargetRange = 5 -- meters

  , bodyAnimation = [ animations.spider01 ]

  , stats = defaultStats //
    { attack      = 5
    , attackRange = 1.2 -- meters
    , defence     = 2
    , maxHealth   = 10
    , maxSpeed    = constants.slowWalkingSpeed
    }

  , unitType = defaultUnitType //
    { attackSpeed = 1   -- seconds
    , pursueRange = 10  -- meters
    }
  , labelOffset = [0.0, 0.8]
  }

let action =
  λ(n : Text) →
  λ(v : List InteractionEffect) →
    { mapKey = n, mapValue = v }

let npcBertram = defaultHumanNPCAgent //
  { name       = names.npcBertram
  , scriptName = ScriptName.Bertram

  , bodyAnimation =
    [ animations.maleBodyLight
    , animations.malePantsTeal
    , animations.maleShirtWhite
    , animations.maleBeardBrown
    , animations.maleHairBangsLongBrown
    ]

  , stats = defaultStats //
    { attack      = 100
    , attackRange = 2 -- meters
    , defence     = 1 -- 100
    , maxHealth   = 10 -- 2000
    , maxSpeed    = constants.fastWalkingSpeed
    }
  , equipmentSlots = constants.defaultEquipmentSlots

  , interactions =
    [ action "Talk to" [ interaction.talkTo ]
    ]
  , primaryInteraction = Some "Talk to"
  }

in
{ bat        = bat
, spider     = spider

, npcBertram = npcBertram
}
