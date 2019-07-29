let sprites    = ./Sprites.dhall
let animations = ./AnimationNames.dhall
let types      = ./Types.dhall
let enums      = ./Enums.dhall
let appearance = ./Appearance.dhall
let useEffect  = ./UseActionEffect.dhall
let names      = ./PassiveNames.dhall
let constants  = ./Constants.dhall

let Entry           = types.Entry
let PassiveTypeName = types.PassiveTypeName
let Sprite          = types.Sprite
let ContainerType   = types.ContainerType
let PassiveKind     = enums.PassiveKind
let WeaponKind      = enums.WeaponKind
let EquipmentSlot   = enums.EquipmentSlot
let UseActionEffect = types.UseActionEffect
let UseActionEntry  = Entry Text (List UseActionEffect)

--------------------------------------------------------------------------------

let defaultStats = constants.defaultStats

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
  λ(name   : PassiveTypeName) →
  λ(volume : Natural) →
  λ(sprite : Sprite) →
    defaultItemType //
      { name        = name
      , volume      = volume
      , passiveKind = [ PassiveKind.BigItem ]
      , appearance  = appearance.simple sprite
      }

--------------------------------------------------------------------------------

let helmet = defaultItemType //
  { name         = names.helmet
  , volume       = 1.5
  , passiveKind  = [ PassiveKind.Item, PassiveKind.BigItem ]
  , appearance   = appearance.simple sprites.helmet
  , fittingSlots = [ EquipmentSlot.Head ]
  , animation    = animations.helmet
  , stats        = defaultStats // { defence = 2 }
  }

let dagger = defaultItemType //
  { name         = names.dagger
  , volume       = 0.3
  , passiveKind  = [ PassiveKind.Item, PassiveKind.SmallItem ]
  , weaponKind   = Some WeaponKind.Slashing
  , appearance   = appearance.simple sprites.dagger
  , fittingSlots = [ EquipmentSlot.PrimaryWeapon ]
  , animation    = animations.dagger
  , stats        = defaultStats // { attack = 2 }
  }

let spear = defaultItemType //
  { name         = names.spear
  , volume       = 0.5
  , passiveKind  = [ PassiveKind.Item, PassiveKind.BigItem ]
  , weaponKind   = Some WeaponKind.Thrusting
  , appearance   = appearance.simple sprites.spear
  , fittingSlots = [ EquipmentSlot.PrimaryWeapon ]
  , animation    = animations.spear
  , stats        = defaultStats // { attack = 5 }
  }

let bow = defaultItemType //
  { name         = names.bow
  , volume       = 0.5
  , passiveKind  = [ PassiveKind.Item, PassiveKind.BigItem ]
  , weaponKind   = Some WeaponKind.Projecting
  , appearance   = appearance.simple sprites.bow
  , fittingSlots = [ EquipmentSlot.PrimaryWeapon ]
  , animation    = animations.bow
  , stats        = defaultStats // { attack = 3 }
  }

let arrow = defaultItemType //
  { name         = names.arrow
  , volume       = 1
  , passiveKind  = [ PassiveKind.Item, PassiveKind.Projectile, PassiveKind.Arrow ]
  , appearance   = appearance.simple sprites.arrow
  , animation    = animations.arrow
  , stats        = defaultStats // { attack = 2 }
  }

let quiver = defaultItemType //
  { name         = names.quiver
  , volume       = 10
  , passiveKind  = [ PassiveKind.Item, PassiveKind.Container ]
  , appearance   = appearance.simple sprites.quiver
  , fittingSlots = [ EquipmentSlot.PrimaryOther ]
  , animation    = animations.quiver
  , behindBody   = Some True
  , containerType = Some
    { maxVolume  = 15
    , allowKinds = [ PassiveKind.Arrow ]
    , showCount  = True
    }
  }

let emptyFlask = defaultItemType //
  { name        = names.emptyFlask
  , volume      = 0.1
  , passiveKind = [ PassiveKind.Item, PassiveKind.SmallItem ]
  , appearance  = appearance.simple sprites.emptyFlask
  }

let healthPotion = defaultItemType //
  { name        = names.healthPotion
  , volume      = 0.1
  , passiveKind = [ PassiveKind.Item, PassiveKind.SmallItem ]
  , appearance  = appearance.simple sprites.healthPotion
  , useActions  =
      [ action "Use" [ useEffect.heal 5, useEffect.transformInto names.emptyFlask ]
      ]
  }

let bag = defaultItemType //
  { name          = names.bag
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
  { name          = names.humanCorpse
  , volume        = 70
  , renderOffset  = [0.0, 0.8]
  }

let batCorpse    = makeCorpse names.batCorpse    30 sprites.batCorpse
let spiderCorpse = makeCorpse names.spiderCorpse 80 sprites.spiderCorpse01

--------------------------------------------------------------------------------

let defaultStaticType = defaultItemType //
  { zindex = 1002
  }

let woodenChest = defaultStaticType //
  { name       = names.woodenChest
  , volume     = 100
  , appearance = [ appearance.sprite sprites.woodenChest1 ]
  , useActions =
    [ action "Open" [ useEffect.transformInto names.woodenChest_open ]
    ]
  , containerType = Some
    { maxVolume  = 100
    , allowKinds = [ PassiveKind.Item ]
    , showCount  = False
    }
  }

let woodenChest_open = defaultStaticType //
  { name       = names.woodenChest_open
  , volume     = 100
  , appearance = [ appearance.sprite sprites.woodenChest1_open ]
  , useActions =
    [ action "Close" [ useEffect.transformInto names.woodenChest ]
    , action "Inspect contents of" [ useEffect.inspectContent ]
    ]
  , containerType = Some
    { maxVolume  = 100
    , allowKinds = [ PassiveKind.Item ]
    , showCount  = False
    }
  }

let woodenDoor = defaultStaticType //
  { name       = names.woodenDoor
  , volume     = 80
  , appearance = [ appearance.sprite sprites.woodenDoor ]
  , useActions =
    [ action "Open" [ useEffect.transformInto names.woodenDoor_open ]
    ]
  }

let woodenDoor_open = defaultStaticType //
  { name       = names.woodenDoor_open
  , volume     = 80
  , appearance = [ appearance.sprite sprites.woodenDoor_open ]
  , useActions =
    [ action "Close" [ useEffect.transformInto names.woodenDoor ]
    ]
  }


let treeAppearance =
  [ appearance.sprite  sprites.treeTrunk
  , appearance.located [0.0, 2.0] sprites.treeFoliage
  ]

let tree = defaultStaticType //
  { name = names.tree
  , volume = 15000
  , appearance = treeAppearance
  , useActions = [ action "Cut down" [ useEffect.deleteSelf ] ]
  }

--------------------------------------------------------------------------------

let makeDeco =
  λ(name   : PassiveTypeName) →
  λ(sprite : Sprite) → defaultStaticType //
  { name = name
  , volume = 100
  , appearance = [ appearance.sprite sprite ]
  }

let campfire = makeDeco names.campfire sprites.campfire // { zindex = 0 }
let firepit  = makeDeco names.firepit  sprites.firepit

let firewoodPileBig   = makeDeco names.firewoodPileBig   sprites.firewoodPileBig
let firewoodPile      = makeDeco names.firewoodPile      sprites.firewoodPile
let firewoodPileSmall = makeDeco names.firewoodPileSmall sprites.firewoodPileSmall
let firewood          = makeDeco names.firewood          sprites.firewood
let woodChoopingBlock = makeDeco names.woodChoopingBlock sprites.woodChoopingBlock
let treeStump         = makeDeco names.treeStump         sprites.treeStump
let barrel            = makeDeco names.barrel            sprites.barrel

--------------------------------------------------------------------------------

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
, woodenDoor       = woodenDoor
, woodenDoor_open  = woodenDoor_open
, tree             = tree

, firepit           = firepit
, campfire          = campfire
, firewoodPileBig   = firewoodPileBig
, firewoodPile      = firewoodPile
, firewoodPileSmall = firewoodPileSmall
, firewood          = firewood
, woodChoopingBlock = woodChoopingBlock
, treeStump         = treeStump
, barrel            = barrel
}
