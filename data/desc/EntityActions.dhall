let types    = ./Types.dhall
let enums    = ./Enums.dhall

let Location        = types.Location
let EntityValue     = types.EntityValue
let EntityAction    = types.EntityAction
let PassiveTypeName = types.PassiveTypeName
let SelectionEntry  = types.SelectionEntry
let LoadoutEntry    = types.LoadoutEntry

let EquipmentSlot = enums.EquipmentSlot

let setValue =
  λ(x : EntityValue) →
  EntityAction.SetValue { SetValue = x }

let addLoadout =
  λ(x : List LoadoutEntry) →
  EntityAction.AddLoadout { AddLoadout = x }

let valueLocation =
  λ(x : Location) →
  EntityValue.Location { Location = x }

let setLocation =
  λ(x : Double) →
  λ(y : Double) →
  setValue (valueLocation [x, y])

let defaultLoadout =
  { probability  = None Double
  , slot         = None EquipmentSlot
  , countRange   = None (List Natural)
  , selection    = [] : List (SelectionEntry PassiveTypeName)
  }

let simpleLoadout =
  λ(x : PassiveTypeName) → defaultLoadout //
    { selection = [ { probability = None Double, name = x } ] }

let countLoadout =
  λ(c : Natural) →
  λ(n : PassiveTypeName) →
    simpleLoadout n // { countRange = Some [ c, c ] }

let slotLoadout =
  λ(s : EquipmentSlot) →
  λ(n : PassiveTypeName) →
    simpleLoadout n // { slot = Some s }

let loadout =
  { simple  = simpleLoadout
  , count   = countLoadout
  , slot    = slotLoadout
  , default = defaultLoadout
  }

in
{ setLocation = setLocation
, addLoadout  = addLoadout
, loadout     = loadout
}
