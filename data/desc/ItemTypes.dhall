let sprites    = ./Sprites.dhall
let types      = ./Types.dhall
let enums      = ./Enums.dhall
let appearance = ./Appearance.dhall

let ContainerType = types.ContainerType
let ItemKind      = enums.ItemKind
let WeaponKind    = enums.WeaponKind
let EquipmentSlot = enums.EquipmentSlot

let defaultItemType =
  { itemKind      = ItemKind.SmallItem
  , weaponKind    = None Text
  , appearance    = appearance.empty
  , fittingSlots  = [] : List Text
  , containerType = None ContainerType
  }

in
{ helmet = defaultItemType //
  { name         = "Helmet"
  , volume       = 1.5
  , itemKind     = ItemKind.BigItem
  , appearance   = appearance.simple sprites.helmet
  , fittingSlots = [ EquipmentSlot.Head ]
  , animation    = Some "helmet"
  }

, dagger = defaultItemType //
  { name         = "Dagger"
  , volume       = 0.3
  , itemKind     = ItemKind.SmallItem
  , weaponKind   = Some WeaponKind.Slashing
  , appearance   = appearance.simple sprites.dagger
  , fittingSlots = [ EquipmentSlot.Weapon ]
  , animation    = Some "dagger"
  }

, spear = defaultItemType //
  { name         = "Spear"
  , volume       = 0.5
  , itemKind     = ItemKind.BigItem
  , weaponKind   = Some WeaponKind.Thrusting
  , appearance   = appearance.simple sprites.spear
  , fittingSlots = [ EquipmentSlot.Weapon ]
  , animation    = Some "spear"
  }

, bow = defaultItemType //
  { name         = "Bow"
  , volume       = 0.5
  , itemKind     = ItemKind.BigItem
  , weaponKind   = Some WeaponKind.Projecting
  , appearance   = appearance.simple sprites.bow
  , fittingSlots = [ EquipmentSlot.Weapon ]
  , animation    = Some "bow"
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

, batCorpse = defaultItemType //
  { name       = "Corpse of a Bat"
  , volume     = 30
  , itemKind   = ItemKind.BigItem
  , appearance = appearance.simple sprites.batCorpse
  }
}
