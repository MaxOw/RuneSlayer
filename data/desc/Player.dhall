let constants = ./Constants.dhall
let passives  = ./PassiveTypes.dhall

let defaultBody =
  [ "maleBodyLight"
  , "maleHairPlainBrown"
  , "malePantsTeal"
  , "maleShirtWhite"
  ]

in
{ body        = defaultBody
, reactivity  = { Life = 0.1 }
, attackRange = 2.0
, maxSpeed    = constants.fastWalkingSpeed
, corpse      = passives.humanCorpse.name
, stats       =
  { attack    = 1
  , defence   = 1
  , maxHealth = 10
  }
}
