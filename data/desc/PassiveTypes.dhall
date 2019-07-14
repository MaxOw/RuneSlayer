let sprites    = ./Sprites.dhall
let types      = ./Types.dhall
let enums      = ./Enums.dhall
let appearance = ./Appearance.dhall
let useEffect  = ./UseActionEffect.dhall

let Entry           = ./PreludeEntryType.dhall
let PassiveTypeName = types.PassiveTypeName
let Sprite          = types.Sprite
let ContainerType   = types.ContainerType
let PassiveKind     = enums.PassiveKind
let WeaponKind      = enums.WeaponKind
let EquipmentSlot   = enums.EquipmentSlot
let UseActionEffect = types.UseActionEffect
let UseActionEntry  = Entry Text (List UseActionEffect)

let makeName = PassiveTypeName.MakeName

let defaultStats =
  { attack    = 0
  , defence   = 0
  , maxHealth = 0
  }

let defaultItemType =
  { passiveKind   = [] : List PassiveKind
  , weaponKind    = None WeaponKind
  , appearance    = appearance.empty
  , fittingSlots  = [] : List EquipmentSlot
  , containerType = None ContainerType
  , stats         = defaultStats
  , useActions    = [] : List UseActionEntry
  , zindex        = 0
  }

let action =
  λ(n : Text) →
  λ(v : List UseActionEffect) →
    { mapKey = n, mapValue = v }

let makeCorpse =
  λ(name   : Text) →
  λ(volume : Natural) →
  λ(sprite : Sprite) →
    defaultItemType //
      { name        = makeName "Corpse of a ${name}"
      , volume      = volume
      , passiveKind = [ PassiveKind.BigItem ]
      , appearance  = appearance.simple sprite
      }

let helmet = defaultItemType //
  { name         = makeName "Helmet"
  , volume       = 1.5
  , passiveKind  = [ PassiveKind.Item, PassiveKind.BigItem ]
  , appearance   = appearance.simple sprites.helmet
  , fittingSlots = [ EquipmentSlot.Head ]
  , animation    = Some "helmet"
  , stats        = defaultStats // { defence = 2 }
  }

let dagger = defaultItemType //
  { name         = makeName "Dagger"
  , volume       = 0.3
  , passiveKind  = [ PassiveKind.Item, PassiveKind.SmallItem ]
  , weaponKind   = Some WeaponKind.Slashing
  , appearance   = appearance.simple sprites.dagger
  , fittingSlots = [ EquipmentSlot.PrimaryWeapon ]
  , animation    = Some "dagger"
  , stats        = defaultStats // { attack = 2 }
  }

let spear = defaultItemType //
  { name         = makeName "Spear"
  , volume       = 0.5
  , passiveKind  = [ PassiveKind.Item, PassiveKind.BigItem ]
  , weaponKind   = Some WeaponKind.Thrusting
  , appearance   = appearance.simple sprites.spear
  , fittingSlots = [ EquipmentSlot.PrimaryWeapon ]
  , animation    = Some "spear"
  , stats        = defaultStats // { attack = 5 }
  }

let bow = defaultItemType //
  { name         = makeName "Bow"
  , volume       = 0.5
  , passiveKind  = [ PassiveKind.Item, PassiveKind.BigItem ]
  , weaponKind   = Some WeaponKind.Projecting
  , appearance   = appearance.simple sprites.bow
  , fittingSlots = [ EquipmentSlot.PrimaryWeapon ]
  , animation    = Some "bow"
  , stats        = defaultStats // { attack = 3 }
  }

let arrow = defaultItemType //
  { name         = makeName "Arrow"
  , volume       = 1
  , passiveKind  = [ PassiveKind.Item, PassiveKind.Projectile, PassiveKind.Arrow ]
  , appearance   = appearance.simple sprites.arrow
  , animation    = Some "arrow"
  , stats        = defaultStats // { attack = 2 }
  }

let quiver = defaultItemType //
  { name         = makeName "Quiver"
  , volume       = 10
  , passiveKind  = [ PassiveKind.Item, PassiveKind.Container ]
  , appearance   = appearance.simple sprites.quiver
  , fittingSlots = [ EquipmentSlot.PrimaryOther ]
  , animation    = Some "quiver"
  , behindBody   = Some True
  , containerType = Some
    { maxVolume  = 15
    , allowKinds = [ PassiveKind.Arrow ]
    , showCount  = True
    }
  }

let emptyFlask = defaultItemType //
  { name        = makeName "Empty Flask"
  , volume      = 0.1
  , passiveKind = [ PassiveKind.Item, PassiveKind.SmallItem ]
  , appearance  = appearance.simple sprites.emptyFlask
  }

let healthPotion = defaultItemType //
  { name        = makeName "Health Potion"
  , volume      = 0.1
  , passiveKind = [ PassiveKind.Item, PassiveKind.SmallItem ]
  , appearance  = appearance.simple sprites.healthPotion
  , useActions  =
      [ action "Use" [ useEffect.heal 5, useEffect.transformInto emptyFlask.name ]
      ]
  }

let bag = defaultItemType //
  { name          = makeName "Bag"
  , volume        = 15
  , passiveKind   = [ PassiveKind.Item, PassiveKind.Container ]
  , appearance    = appearance.simple sprites.bag
  , fittingSlots  = [ EquipmentSlot.Backpack ]
  , containerType = Some
    { maxVolume  = 15
    , allowKinds = [ PassiveKind.SmallItem ]
    , showCount  = False
    }
  }

let humanCorpse = defaultItemType //
  { name          = makeName "Human Corpse"
  , volume        = 70
  -- , passiveKind   = [ ]
  }

let batCorpse    = makeCorpse "Bat"    30 sprites.batCorpse
let spiderCorpse = makeCorpse "Spider" 80 sprites.spiderCorpse01

--------------------------------------------------------------------------------

let defaultStaticType = defaultItemType //
  { zindex = 1002
  }

-- Dhall doesn't allow for mutually referencing let bindings,
-- so we heve to do this...
let woodenChest_open_name = makeName "Open Wooden Chest"

let woodenChest = defaultStaticType //
  { name       = makeName "Wooden Chest"
  , volume     = 100
  , appearance = [ appearance.sprite sprites.woodenChest1 ]
  , useActions =
    [ action "Open" [ useEffect.transformInto woodenChest_open_name ]
    ]
  }

let woodenChest_open = defaultStaticType //
  { name       = woodenChest_open_name
  , volume     = 100
  , appearance = [ appearance.sprite sprites.woodenChest1_open ]
  , useActions =
    [ action "Close" [ useEffect.transformInto woodenChest.name ]
    , action "Inspect contents of" [ useEffect.inspectContent ]
    ]
  , containerType = Some
    { maxVolume  = 100
    , allowKinds = [ PassiveKind.Item ]
    , showCount  = False
    }
  }

let treeAppearance =
  [ appearance.sprite  sprites.treeTrunk
  , appearance.located [0.0, 2.0] sprites.treeFoliage
  ]

let tree = defaultStaticType //
  { name = makeName "Tree"
  , volume = 15000
  , appearance = treeAppearance
  , useActions = [ action "Cut down" [ useEffect.deleteSelf ] ]
  }

in
{ helmet           = helmet
, dagger           = dagger
, spear            = spear
, bow              = bow
, arrow            = arrow
, quiver           = quiver
, healthPotion     = healthPotion
, emptyFlask       = emptyFlask
, bag              = bag
, batCorpse        = batCorpse
, spiderCorpse     = spiderCorpse
, humanCorpse      = humanCorpse

, woodenChest      = woodenChest
, woodenChest_open = woodenChest_open
, tree             = tree
}
