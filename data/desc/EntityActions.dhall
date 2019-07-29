let types    = ./Types.dhall
let enums    = ./Enums.dhall

let PassiveTypeName = types.PassiveTypeName
let AgentTypeName   = types.AgentTypeName
let Probability     = types.Probability
let LoadoutEntry    = types.LoadoutEntry
let Range           = types.Range
let EntityValue     = types.EntityValue
let EntityActionF   = types.EntityActionF
let EntityAction    = types.EntityAction

let EquipmentSlot = enums.EquipmentSlot
let Direction     = enums.Direction

let defaultLoadoutEntry =
  { probability = None Probability
  , slot        = None EquipmentSlot
  , countRange  = None Range
  }

let MakeEntityAction
  = λ(t : Type)
  → λ(fix : EntityActionF t → t)
  → let E = EntityActionF t in
    { SetValue   = λ(x : EntityValue)           → fix (E.SetValueF   { _1 = x })
    , AddLoadout = λ(x : List (LoadoutEntry t)) → fix (E.AddLoadoutF { _1 = x })
    }

let simpleLoadout
  = λ(t : Type)
  → λ(x : PassiveTypeName)
  → defaultLoadoutEntry //
    { selection = [
      { probability = None Probability
      , entry = { name = x, actions = [] : List t }
      } ]
    }

let simpleLoadoutWith
  = λ(t : Type)
  → λ(x : PassiveTypeName)
  → λ(a : List t)
  → defaultLoadoutEntry //
    { selection = [
      { probability = None Probability
      , entry = { name = x, actions = a }
      } ]
    }

let countExact
  = λ(c : Natural)
  → Some { rangeMin = c, rangeMax = c }

let countLoadout
  = λ(t : Type)
  → λ(c : Natural)
  → λ(n : PassiveTypeName)
  → simpleLoadout t n // { countRange = countExact c }

let addLoadout
  = λ(t : Type)
  → λ(fix : EntityActionF t → t)
  → λ(x : List (LoadoutEntry t))
  → (MakeEntityAction t fix).AddLoadout x

let addLoadoutCount
  = λ(c : Natural)
  → λ(i : PassiveTypeName)
  → λ(t : Type)
  → λ(fix : EntityActionF t → t)
  → addLoadout t fix [ countLoadout t c i ]

let containerWithCountLoadout
  = λ(t : Type)
  → λ(fix : EntityActionF t → t)
  → λ(n : PassiveTypeName)
  → λ(c : Natural)
  → λ(i : PassiveTypeName)
  → simpleLoadoutWith t n
      [ addLoadout t fix [ countLoadout t c i ] ]

let makeLoadout
  = λ(t : Type)
  → λ(fix : EntityActionF t → t)
  → { simple             = simpleLoadout t
    , simpleWith         = simpleLoadoutWith t
    , count              = countLoadout t
    , containerWithCount = containerWithCountLoadout t fix
    }

let setLocation
  = λ(x : Double)
  → λ(y : Double)
  → λ(t : Type)
  → λ(fix : EntityActionF t → t)
  → let E = MakeEntityAction t fix in
    E.SetValue (EntityValue.Location { _1 = { x = x, y = y } })

let makeFix
  = λ(t : Type)
  → λ(fix : EntityActionF t → t)
  → let E = MakeEntityAction t fix in
    { setLocation = λ(x : Double) → λ(y : Double) → setLocation x y t fix
    , addLoadout  = E.AddLoadout
    , loadout     = makeLoadout t fix
    }

in
{ setLocation     = setLocation
, addLoadoutCount = addLoadoutCount
, makeFix         = makeFix
}

