let sprites    = ./Sprites.dhall
let types      = ./Types.dhall
let enums      = ./Enums.dhall
let appearance = ./Appearance.dhall

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
  { itemKind      = ItemKind.SmallItem
  , weaponKind    = None Text
  , appearance    = appearance.empty
  , fittingSlots  = [] : List Text
  , containerType = None ContainerType
  , stats         = defaultStats
  }

let makeCorpse =
  λ(name   : Text) →
  λ(volume : Natural) →
  λ(sprite : Sprite) →
    defaultItemType //
      { name       = "Corpse of a ${name}"
      , volume     = volume
      , itemKind   = ItemKind.BigItem
      , appearance = appearance.simple sprite
      }

in
{ helmet = defaultItemType //
  { name          = "Helmet"
  , volume        = 1.5
  , itemKind      = ItemKind.BigItem
  , appearance    = appearance.simple sprites.helmet
  , fittingSlots  = [ EquipmentSlot.Head ]
  , animation     = Some "helmet"
  , stats         = defaultStats // { defence = 2 }
  }

, dagger = defaultItemType //
  { name         = "Dagger"
  , volume       = 0.3
  , itemKind     = ItemKind.SmallItem
  , weaponKind   = Some WeaponKind.Slashing
  , appearance   = appearance.simple sprites.dagger
  , fittingSlots = [ EquipmentSlot.PrimaryWeapon ]
  , animation    = Some "dagger"
  , stats        = defaultStats // { attack = 2 }
  }

, spear = defaultItemType //
  { name         = "Spear"
  , volume       = 0.5
  , itemKind     = ItemKind.BigItem
  , weaponKind   = Some WeaponKind.Thrusting
  , appearance   = appearance.simple sprites.spear
  , fittingSlots = [ EquipmentSlot.PrimaryWeapon ]
  , animation    = Some "spear"
  , stats        = defaultStats // { attack = 5 }
  }

, bow = defaultItemType //
  { name         = "Bow"
  , volume       = 0.5
  , itemKind     = ItemKind.BigItem
  , weaponKind   = Some WeaponKind.Projecting
  , appearance   = appearance.simple sprites.bow
  , fittingSlots = [ EquipmentSlot.PrimaryWeapon ]
  , animation    = Some "bow"
  , stats        = defaultStats // { attack = 3 }
  }

, arrow = defaultItemType //
  { name         = "Arrow"
  , volume       = 0.2
  , itemKind     = ItemKind.Projectile
  , appearance   = appearance.simple sprites.arrow
  , fittingSlots = [ EquipmentSlot.PrimaryOther ]
  , animation    = Some "arrow"
  , stats        = defaultStats // { attack = 2 }
  }

, healthPotion = defaultItemType //
  { name       = "Health Potion"
  , volume     = 0.1
  , itemKind   = ItemKind.SmallItem
  , appearance = appearance.simple sprites.healthPotion
  }

, bag = defaultItemType //
  { name          = "Bag"
  , volume        = 15
  , itemKind      = ItemKind.Container
  , appearance    = appearance.simple sprites.bag
  , fittingSlots  = [ EquipmentSlot.Backpack ]
  , containerType = Some { maxVolume              = 15 }
  }

, batCorpse    = makeCorpse "Bat"    30 sprites.batCorpse
, spiderCorpse = makeCorpse "Spider" 80 sprites.spiderCorpse01
}
