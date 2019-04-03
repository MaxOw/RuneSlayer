let sprites    = ./Sprites.dhall
let types      = ./Types.dhall
let enums      = ./Enums.dhall
let appearance = ./Appearance.dhall

let ContainerType = types.ContainerType
let ItemKind      = enums.ItemKind
let EquipmentSlot = enums.EquipmentSlot

let defaultItemType =
  { itemKind = ItemKind.SmallItem
  , appearance = appearance.empty
  , fittingSlots = [] : List Text
  , containerType = None ContainerType
  }

in
{ helmet = defaultItemType //
  { name     = "Helmet"
  , volume   = 1.5
  , itemKind = ItemKind.BigItem
  , appearance = appearance.simple sprites.helmet
  , fittingSlots = [ EquipmentSlot.Head ]
  }

, healthPotion = defaultItemType //
  { name     = "Health Potion"
  , volume   = 0.1
  , itemKind = ItemKind.SmallItem
  , appearance = appearance.simple sprites.healthPotion
  }

, bag = defaultItemType //
  { name     = "Bag"
  , volume   = 15
  , itemKind = ItemKind.Container
  , appearance = appearance.simple sprites.bag
  , fittingSlots = [ EquipmentSlot.Backpack ]
  , containerType = Some { maxVolume = 15 }
  }

, batCorpse = defaultItemType //
  { name     = "Corpse of a Bat"
  , volume   = 30
  , itemKind = ItemKind.BigItem
  , appearance = appearance.simple sprites.batCorpse
  }
}
