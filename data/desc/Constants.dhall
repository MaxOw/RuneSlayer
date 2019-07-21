
let defaultStats =
  { attack    = 0
  , defence   = 0
  , maxHealth = 0
  , maxSpeed  = 0
  }

in
{ slowWalkingSpeed   = 1.0 -- m/s
, baseWalkingSpeed   = 2.0 -- m/s
, fastWalkingSpeed   = 3.0 -- m/s
, baseRunningSpeed   = 6.0 -- m/s
, baseSprintingSpeed = 8.0 -- m/s

, defaultStats = defaultStats
, humanReactivity = { Life = 0.1 }
}
