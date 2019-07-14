let types = ./Types.dhall

let UseActionEffect = types.UseActionEffect
let PassiveTypeName = types.PassiveTypeName

let unit = [] : List {}

let transformInto =
  λ(x : PassiveTypeName) →
    UseActionEffect.TransformInto { TransformInto = x }

let deleteSelf     = UseActionEffect.DeleteSelf     { DeleteSelf     = unit }
let inspectContent = UseActionEffect.InspectContent { InspectContent = unit }

let heal =
  λ(x : Natural) →
    UseActionEffect.Heal { Heal = x }
in
{ transformInto  = transformInto
, deleteSelf     = deleteSelf
, inspectContent = inspectContent
, heal           = heal
}

