let sprites    = ./Sprites.dhall
let animations = ./AnimationNames.dhall
let types      = ./Types.dhall
let enums      = ./Enums.dhall
let appearance = ./Appearance.dhall
let useEffect  = ./InteractionEffect.dhall
let names      = ./PassiveNames.dhall
let constants  = ./Constants.dhall
let collision  = ./CollisionShape.dhall

let Entry             = types.Entry
let PassiveTypeName   = types.PassiveTypeName
let Sprite            = types.Sprite
let ContainerType     = types.ContainerType
let CollideWith       = types.CollideWith
let PassiveKind       = enums.PassiveKind
let WeaponKind        = enums.WeaponKind
let EquipmentSlot     = enums.EquipmentSlot
let InteractionEffect = types.InteractionEffect
let InteractionEntry  = Entry Text (List InteractionEffect)

--------------------------------------------------------------------------------

let defaultStats = constants.defaultStats

let defaultItemType =
  { passiveKind        = [] : List PassiveKind
  , weaponKind         = None WeaponKind
  , appearance         = appearance.empty
  , fittingSlots       = [] : List EquipmentSlot
  , containerType      = None ContainerType
  , stats              = defaultStats
  , interactions       = [] : List InteractionEntry
  , primaryInteraction = None Text
  , zindex             = 0
  , standingWeight     = 10000.0
  , collisionBits      = [] : List CollideWith
  }

let action =
  λ(n : Text) →
  λ(v : List InteractionEffect) →
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

let weaponSlots = [ EquipmentSlot.PrimaryWeapon, EquipmentSlot.SecondaryWeapon ]
let otherSlots  = [ EquipmentSlot.PrimaryOther,  EquipmentSlot.SecondaryOther  ]

--------------------------------------------------------------------------------

let helmet = defaultItemType //
  { name         = names.helmet
  , description  = Some "Fully enclosing metal helmet with a pivoting visor."
  , volume       = 1.5
  , passiveKind  = [ PassiveKind.Item, PassiveKind.BigItem ]
  , appearance   = appearance.simple sprites.helmet
  , fittingSlots = [ EquipmentSlot.Head ]
  , animation    = animations.helmet
  , stats        = defaultStats // { defence = 2 }
  }

let dagger = defaultItemType //
  { name         = names.dagger
  , description  = Some "Simple dagger with a leather bound handle."
  , volume       = 0.3
  , passiveKind  = [ PassiveKind.Item, PassiveKind.SmallItem ]
  , weaponKind   = Some WeaponKind.Slashing
  , appearance   = appearance.simple sprites.dagger
  , fittingSlots = weaponSlots
  , animation    = animations.dagger
  , stats        = defaultStats // { attack = 2, attackRange = 2 }
  }

let spear = defaultItemType //
  { name         = names.spear
  , description  = Some "Long piece of wood with a pointy metal tip."
  , volume       = 0.5
  , passiveKind  = [ PassiveKind.Item, PassiveKind.BigItem ]
  , weaponKind   = Some WeaponKind.Thrusting
  , appearance   = appearance.simple sprites.spear
  , fittingSlots = weaponSlots
  , animation    = animations.spear
  , stats        = defaultStats // { attack = 5, attackRange = 3 }
  }

let sword = defaultItemType //
  { name         = names.sword
  , description  = "One and a half handed sword. Quite long and simple in design."
  , volume       = 0.5
  , passiveKind  = [ PassiveKind.Item, PassiveKind.BigItem ]
  , weaponKind   = Some WeaponKind.Slashing
  , appearance   = appearance.simple sprites.sword
  , fittingSlots = weaponSlots
  , animation    = animations.sword
  , stats        = defaultStats // { attack = 5, attackRange = 3 }
  }

let bow = defaultItemType //
  { name         = names.bow
  , description  = Some "Straight self bow with bowstring made of rawhide."
  , volume       = 0.5
  , passiveKind  = [ PassiveKind.Item, PassiveKind.BigItem ]
  , weaponKind   = Some WeaponKind.Projecting
  , appearance   = appearance.simple sprites.bow
  , fittingSlots = weaponSlots
  , animation    = animations.bow
  , stats        = defaultStats // { attack = 3, attackRange = 9 }
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
  , description  = Some "Simple leather quiver."
  , volume       = 10
  , passiveKind  = [ PassiveKind.Item, PassiveKind.SmallItem, PassiveKind.Container ]
--, passiveKind  = [ PassiveKind.Item, PassiveKind.Container ]
  , appearance   = appearance.simple sprites.quiver
  , fittingSlots = otherSlots
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
  , description = Some "Small empty glass flask."
  , volume      = 0.1
  , passiveKind = [ PassiveKind.Item, PassiveKind.SmallItem ]
  , appearance  = appearance.simple sprites.emptyFlask
  }

let healthPotionPower = 5
let healthPotionDescription = ''
    Small glass flask containing red liquid. Restores
    ${Natural/show healthPotionPower} health points on use.
  ''
let healthPotion = defaultItemType //
  { name        = names.healthPotion
  , description = Some healthPotionDescription
  , volume      = 0.1
  , passiveKind = [ PassiveKind.Item, PassiveKind.SmallItem ]
  , appearance  = appearance.simple sprites.healthPotion
  , interactions =
      [ action "Use"
        [ useEffect.heal healthPotionPower
        , useEffect.transformInto names.emptyFlask ]
      ]
  , primaryInteraction = Some "Use"
  , labelOffset = [0.0, 0.8]
  }

let bagMaxVolume = 15
let bagDescription = ''
    Leather bag with a belt worn sling across over one shoulder.
    Has volume of up to ${Natural/show bagMaxVolume} liters.
  ''
let bag = defaultItemType //
  { name          = names.bag
  , description   = Some bagDescription
  , volume        = 15
  , passiveKind   = [ PassiveKind.Item, PassiveKind.Container ]
  , appearance    = appearance.simple sprites.bag
  , fittingSlots  = [ EquipmentSlot.Backpack ]
  , containerType = Some
    { maxVolume  = bagMaxVolume
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

let woodenChest_common = defaultStaticType //
  { volume = 100
  , containerType = Some
    { maxVolume  = 100
    , allowKinds = [ PassiveKind.Item ]
    , showCount  = False
    }
  , labelOffset = [0.0, 1.2]
  , renderOffset = [0.0, 0.4]
  -- , collisionShape = collision.translate 0.0 0.2 (collision.circle 0.4)
  , collisionShape = collision.translate 0.0 0.2 (collision.rect 1.0 0.6)
  , collisionBits  = [ CollideWith.Low ]
  }

let inspectActionName = "Inspect"

let woodenChest = woodenChest_common //
  { name       = names.woodenChest
  , appearance = [ appearance.sprite sprites.woodenChest1 ]
  , interactions =
    [ action inspectActionName
      [ useEffect.transformInto names.woodenChest_open
      , useEffect.inspectContent ]
    , action "Open" [ useEffect.transformInto names.woodenChest_open ]
    ]
  , primaryInteraction = Some inspectActionName
  }

let woodenChest_open = woodenChest_common //
  { name       = names.woodenChest_open
  , appearance = [ appearance.sprite sprites.woodenChest1_open ]
  , interactions =
    [ action inspectActionName [ useEffect.inspectContent ]
    , action "Close" [ useEffect.transformInto names.woodenChest ]
    ]
  , primaryInteraction = Some inspectActionName
  }

let woodenDoor = defaultStaticType //
  { name       = names.woodenDoor
  , volume     = 80
  , appearance = [ appearance.sprite sprites.woodenDoor ]
  , interactions =
    [ action "Open" [ useEffect.transformInto names.woodenDoor_open ]
    ]
  , primaryInteraction = Some "Open"
  }

let woodenDoor_open = defaultStaticType //
  { name       = names.woodenDoor_open
  , volume     = 80
  , appearance = [ appearance.sprite sprites.woodenDoor_open ]
  , interactions =
    [ action "Close" [ useEffect.transformInto names.woodenDoor ]
    ]
  , primaryInteraction = Some "Close"
  }


let treeAppearance =
  [ appearance.sprite  sprites.treeTrunk
  , appearance.located [0.0, 2.0] sprites.treeFoliage
  ]

let tree = defaultStaticType //
  { name = names.tree
  , volume = 15000
  , appearance = treeAppearance
  , interactions = [ action "Cut down" [ useEffect.deleteSelf ] ]
  , renderOffset = [0.0, 0.4]
  , collisionShape = collision.translate 0.0 0.15 (collision.circle 0.3)
  , collisionBits  = [ CollideWith.Low, CollideWith.High ]
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

let firewood          = makeDeco names.firewood          sprites.firewood
let firewoodPile      = makeDeco names.firewoodPile      sprites.firewoodPile
let firewoodPileBig   = makeDeco names.firewoodPileBig   sprites.firewoodPileBig
let firewoodPileSmall = makeDeco names.firewoodPileSmall sprites.firewoodPileSmall //
  { collisionShape = collision.translate 0.0 -0.25 (collision.circle 0.3)
  , collisionBits  = [ CollideWith.Low ]
  }

let woodChoopingBlock = makeDeco names.woodChoopingBlock sprites.woodChoopingBlock //
  { collisionShape = collision.translate -0.1 -0.25 (collision.circle 0.3)
  , collisionBits  = [ CollideWith.Low ]
  }
let treeStump = makeDeco names.treeStump sprites.treeStump //
  { collisionShape = collision.translate 0.0 -0.15 (collision.circle 0.3)
  , collisionBits  = [ CollideWith.Low ]
  }
let barrel = makeDeco names.barrel sprites.barrel //
  { renderOffset = [0.0, 0.4]
  , collisionShape = collision.translate 0.0 0.0 (collision.circle 0.4)
  , collisionBits  = [ CollideWith.Low ]
  }

--------------------------------------------------------------------------------

in
{ helmet           = helmet
, dagger           = dagger
, spear            = spear
, sword            = sword
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
