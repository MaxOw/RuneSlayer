let constants = ./Constants.dhall

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
}
