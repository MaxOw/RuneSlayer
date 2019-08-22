let types = ./Types.dhall

let InteractionEffect = types.InteractionEffect
let PassiveTypeName   = types.PassiveTypeName

let unit = [] : List {}

let transformInto =
  λ(x : PassiveTypeName) →
    InteractionEffect.TransformInto { TransformInto = x }

let deleteSelf     = InteractionEffect.DeleteSelf     { DeleteSelf     = unit }
let inspectContent = InteractionEffect.InspectContent { InspectContent = unit }

let heal =
  λ(x : Natural) →
    InteractionEffect.Heal { Heal = x }
in
{ transformInto  = transformInto
, deleteSelf     = deleteSelf
, inspectContent = inspectContent
, heal           = heal
}

