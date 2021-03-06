let types   = ./Types.dhall

let AnimationName = types.AnimationName
let makeName = AnimationName.MakeAnimationName

-- Ideally we would like to have this file autogenerated from a simple list of
-- names, but for now we will write it manually.

in
{ maleBodyLight          = makeName "maleBodyLight"
, malePantsTeal          = makeName "malePantsTeal"
, maleShirtWhite         = makeName "maleShirtWhite"

, maleHairBangsLongBrown = makeName "maleHairBangsLongBrown"
, maleHairPlainBrown     = makeName "maleHairPlainBrown"
, maleBeardBrown         = makeName "maleBeardBrown"

, helmet                 = makeName "helmet"
, dagger                 = makeName "dagger"
, spear                  = makeName "spear"
, sword                  = makeName "sword"
, bow                    = makeName "bow"
, arrow                  = makeName "arrow"
, quiver                 = makeName "quiver"

, bat                    = makeName "bat"
, spider01               = makeName "spider01"
}
