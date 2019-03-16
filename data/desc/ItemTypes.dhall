let sprites = ./Sprites.dhall
let enums   = ./Enums.dhall
let appearance = ./Appearance.dhall

let ItemKind      = enums.ItemKind
let EquipmentSlot = enums.EquipmentSlot

in
{ helmet =
  { name     = "Helmet"
  , volume   = 1.5
  , itemKind = ItemKind.BigItem
  , appearance = appearance.sprite sprites.helmet
  , fittingSlots = [ EquipmentSlot.Head ]
  }

, healthPotion =
  { name     = "Health Potion"
  , volume   = 0.1
  , itemKind = ItemKind.SmallItem
  , appearance = appearance.sprite sprites.healthPotion
  , fittingSlots = [] : List Text
  }
}
