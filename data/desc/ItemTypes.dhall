let sprites    = ./Sprites.dhall
let types      = ./Types.dhall
let enums      = ./Enums.dhall
let appearance = ./Appearance.dhall
let useEffect  = ./ItemUseEffect.dhall

let ItemUseEffect = types.ItemUseEffect
let Sprite        = types.Sprite
let ContainerType = types.ContainerType
let ItemKind      = enums.ItemKind
let WeaponKind    = enums.WeaponKind
let EquipmentSlot = enums.EquipmentSlot

let defaultStats =
  { attack  = 0
  , defence = 0
  }

let defaultItemType =
  { itemKind      = [] : List Text
  , weaponKind    = None Text
  , appearance    = appearance.empty
  , fittingSlots  = [] : List Text
  , containerType = None ContainerType
  , stats         = defaultStats
  , useEffects    = [] : List ItemUseEffect
  }

let makeCorpse =
  λ(name   : Text) →
  λ(volume : Natural) →
  λ(sprite : Sprite) →
    defaultItemType //
      { name       = "Corpse of a ${name}"
      , volume     = volume
      , itemKind   = [ ItemKind.BigItem ]
      , appearance = appearance.simple sprite
      }

let helmet = defaultItemType //
  { name          = "Helmet"
  , volume        = 1.5
  , itemKind      = [ ItemKind.BigItem ]
  , appearance    = appearance.simple sprites.helmet
  , fittingSlots  = [ EquipmentSlot.Head ]
  , animation     = Some "helmet"
  , stats         = defaultStats // { defence = 2 }
  }

let dagger = defaultItemType //
  { name         = "Dagger"
  , volume       = 0.3
  , itemKind     = [ ItemKind.SmallItem ]
  , weaponKind   = Some WeaponKind.Slashing
  , appearance   = appearance.simple sprites.dagger
  , fittingSlots = [ EquipmentSlot.PrimaryWeapon ]
  , animation    = Some "dagger"
  , stats        = defaultStats // { attack = 2 }
  }

let spear = defaultItemType //
  { name         = "Spear"
  , volume       = 0.5
  , itemKind     = [ ItemKind.BigItem ]
  , weaponKind   = Some WeaponKind.Thrusting
  , appearance   = appearance.simple sprites.spear
  , fittingSlots = [ EquipmentSlot.PrimaryWeapon ]
  , animation    = Some "spear"
  , stats        = defaultStats // { attack = 5 }
  }

let bow = defaultItemType //
  { name         = "Bow"
  , volume       = 0.5
  , itemKind     = [ ItemKind.BigItem ]
  , weaponKind   = Some WeaponKind.Projecting
  , appearance   = appearance.simple sprites.bow
  , fittingSlots = [ EquipmentSlot.PrimaryWeapon ]
  , animation    = Some "bow"
  , stats        = defaultStats // { attack = 3 }
  }

let arrow = defaultItemType //
  { name         = "Arrow"
  , volume       = 1
  , itemKind     = [ ItemKind.Projectile, ItemKind.Arrow ]
  , appearance   = appearance.simple sprites.arrow
  , fittingSlots = [ EquipmentSlot.PrimaryOther ]
  , animation    = Some "arrow"
  , stats        = defaultStats // { attack = 2 }
  }

let quiver = defaultItemType //
  { name         = "Quiver"
  , volume       = 10
  , itemKind     = [ ItemKind.Container ]
  , appearance   = appearance.simple sprites.quiver
  , fittingSlots = [ EquipmentSlot.Quiver ]
  , animation    = Some "quiver"
  , containerType = Some
    { maxVolume  = 15
    , allowKinds = [ ItemKind.Arrow ]
    , showCount  = True
    }
  }

let emptyFlask = defaultItemType //
  { name       = "Empty Flask"
  , volume     = 0.1
  , itemKind   = [ ItemKind.SmallItem ]
  , appearance = appearance.simple sprites.emptyFlask
  }

let healthPotion = defaultItemType //
  { name       = "Health Potion"
  , volume     = 0.1
  , itemKind   = [ ItemKind.SmallItem ]
  , appearance = appearance.simple sprites.healthPotion
  , useEffects = [ useEffect.heal 5, useEffect.transformInto emptyFlask.name ]
  }

let bag = defaultItemType //
  { name          = "Bag"
  , volume        = 15
  , itemKind      = [ ItemKind.Container ]
  , appearance    = appearance.simple sprites.bag
  , fittingSlots  = [ EquipmentSlot.Backpack ]
  , containerType = Some
    { maxVolume  = 15
    , allowKinds = [ ItemKind.SmallItem ]
    , showCount  = False
    }
  }

let batCorpse    = makeCorpse "Bat"    30 sprites.batCorpse
let spiderCorpse = makeCorpse "Spider" 80 sprites.spiderCorpse01

in
{ helmet       = helmet
, dagger       = dagger
, spear        = spear
, bow          = bow
, arrow        = arrow
, quiver       = quiver
, healthPotion = healthPotion
, emptyFlask   = emptyFlask
, bag          = bag
, batCorpse    = batCorpse
, spiderCorpse = spiderCorpse
}
