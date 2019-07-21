let types    = ./Types.dhall

let Location     = types.Location
let EntityValue  = types.EntityValue
let EntityAction = types.EntityAction

let setValue =
  λ(v : EntityValue) →
  EntityAction.SetValue { SetValue = v }

let valueLocation =
  λ(l : Location) →
  EntityValue.Location { Location = l }

let setLocation =
  λ(l : Location) →
  setValue (valueLocation l)

in
{ setLocation = setLocation
}
